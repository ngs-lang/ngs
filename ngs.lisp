;; apt-get install cl-alexandria cl-esrap cl-ppcre cl-yason cl-trivial-gray-streams

(require :asdf)
(require :sb-posix)
(require :alexandria)
(require :esrap)
(require :cl-ppcre)
(require :yason)

(defpackage :ngs
  (:use :cl :esrap)
  (:export
   :ngs-call-function
   :ngs-compile
   :update-runtime-info))

(in-package :ngs)

(defparameter *read-buffer-size* 4096)

(defstruct parameters positional named)
(defstruct arguments  positional named)

(defparameter *running-as-binary* nil)

(defvar *source-file-name* "TOP-LEVEL")
(defvar *source-file-positions* #(0))

;; Parser - start ------------------------------


(defclass node ()
  ((src :initarg :src :initform nil :accessor node-src)
   (children :initarg :children :initform (list) :accessor node-children)
   (data :initarg :data :initform nil :accessor node-data)))

(defparameter *high-priority-optional-space-operators*
  '(("|" "@?" "@")))

(defparameter *required-space-binary-operators*
  '(("is not" "is")
    ("throws")
    ("returns")
    ("or")
    ("and")
    ("in" "not in")))

(defparameter *optional-space-binary-operations*
  '(("===" "!==" "==" "!=" "<" ">" "~~" "~")
    ("+" "-")
    ("*" "/")))

(defparameter *binary-functions*
  `("is not" "is" "in" "not in" "[]=" "[]" ".=" "." "$()" "````" "``" "|" ,@(alexandria:flatten *optional-space-binary-operations*)))

(defparameter *binary-operators*
  (append
   (mapcar #'(lambda(x) `(and optional-space (or ,@x) optional-space)) *high-priority-optional-space-operators*)
   (mapcar #'(lambda(x) `(and space (or ,@x) space)) *required-space-binary-operators*)
   (mapcar #'(lambda(x) `(and optional-space (or ,@x) optional-space)) *optional-space-binary-operations*)))

(defparameter *binary-operators-precedence*
  (let ((h (make-hash-table :test 'equal :size 30)))
    (loop
       for group in (append *high-priority-optional-space-operators* *required-space-binary-operators* *optional-space-binary-operations*)
       for idx from 0
       do (loop for op in group
             do (setf (gethash op h) idx)))
    h))

(defparameter *regexp-flags*
  '((#\i . :case-insensitive-mode)
    (#\m . :multi-line-mode)
    (#\s . :single-line-mode)
    (#\e . :extended-mode)))

(defmethod print-object ((n node) stream)
  (format stream "#<~A :SRC ~A :DATA ~A :CHILDREN ~A>"
          (class-name (class-of n))
          (node-src n)
          (node-data n)
          (node-children n)))

(defclass number-node (node) ())
(defclass string-node (node) ())
(defclass string-container-node (node) ())
(defclass binary-operation-node (node) ())
(defclass identifier-node (node) ())
(defclass varname-node (node) ())
(defclass assignment-node (node) ())
(defclass expressions-node (node) ())

(defclass function-definition-node (node) ())
(defclass lambda-node (node) ())
(defclass function-parameter-node (node) ())
(defclass function-parameters-node (node) ())
(defclass function-argument-node (node) ())
(defclass function-arguments-node (node) ())
(defclass function-call-node (node) ())

(defclass incompilable-node (node) ())
(defclass comment-node (incompilable-node) ())
(defclass end-node (incompilable-node) ())

(defclass keyword-node (node) ())

(defclass list-node (node) ())
(defclass array-node (node) ())
(defclass splice-node (node) ())
(defclass list-concat-node (node) ())
(defclass array-concat-node (node) ())
(defclass kv-splice-node (node) ())
(defclass kv-pair-node (node) ())
(defclass hash-node (node) ())
(defclass getattr-node (node) ())
(defclass setattr-node (node) ())
(defclass getitem-node (node) ())
(defclass setitem-node (node) ())
(defclass if-node (node) ())
(defclass try-catch-node (node) ())
(defclass throw-node (node) ())
(defclass for-node (node) ())
(defclass while-node (node) ())
(defclass guard-node (node) ())
(defclass return-node (node) ())
(defclass literal-node (node) ())
(defclass regexp-node (node) ())
(defclass command-node (node) ())
(defclass command-code-node (node) ())
(defclass commands-node (node) ())

(defun make-binop-node (ls)
  (let ((result (first ls)))
    (loop
       for (op expr) in (second ls)
       do (setq result (make-instance 'binary-operation-node :data (second op) :children (list result expr))))
    result))

(defun process-possible-splice (result-type node)
  (if (some #'(lambda (x) (typep x 'splice-node)) (node-children node))
      (make-instance
       result-type
       :children
       (mapcar #'(lambda (x)
                   (if (typep x 'splice-node)
                       (first (node-children x))
                       (make-instance 'list-node :children (list x))))
               (node-children node)))
      node))


(defun make-human-position (position)
  (let ((line (loop
                 for p across *source-file-positions*
                 for line from 0
                 if (or (eq line (1- (length *source-file-positions*)))
                        (> (elt *source-file-positions* (1+ line)) position)) return line)))
    (format nil "~A:~A:~A" *source-file-name* (1+ line) (1+ (- position (elt *source-file-positions* line))))))

;; (define-symbol-macro %std-src (list 'list (make-human-position start) (make-human-position end)))
(define-symbol-macro %std-src (format nil "~A-~A" (make-human-position start) (make-human-position end)))

(defrule comment (and #\# (* (and (! #\Newline) character)))
  (:lambda (list &bounds start end)
    (declare (ignore list))
    (make-instance 'comment-node :src %std-src)))

(defrule end (and "END" #\Newline (* (string 1)))
  (:lambda (list &bounds start end)
    (declare (ignore list))
    (make-instance 'end-node :src %std-src)))

(defrule optional-sign (or "+" "-" ""))

(defrule digit (character-ranges (#\0 #\9)))

(defrule digits (+ digit))

(defrule hex-digit (or digit (character-ranges (#\a #\f) (#\A #\F))))

(defrule hex-digits (+ hex-digit))

(defrule inline-space (+ (or #\Space #\Tab)) (:constant nil))

(defrule space (+ (or #\Space #\Tab #\Newline)) (:constant nil))

(defrule newline-space (+ (and (? inline-space) (+ #\Newline) (? inline-space))) (:constant nil))

(defrule optional-space (* space) (:constant nil))

(defrule integer (and optional-sign digits) (:lambda (list) (parse-integer (text list) :radix 10)))

(defrule float (and optional-sign digits "." digits) (:lambda (list) (with-input-from-string (s (text list)) (read s))))

(defun make-integer-if-possible (x)
  (multiple-value-bind (result remainder) (floor x)
    (if (zerop remainder)
        result
        x)))

;; Units idea - Thanks to Avishai Ish Shalom
(defrule number (and (or float integer) (? (or #\K #\M #\G)))
  (:lambda (n)
    (make-instance
     'number-node
     :data (make-integer-if-possible (* (first n)
                                        (let ((units (second n)))
                                          ;; (format t "UNITS ~S~%" units)
                                          (cond
                                            ((equal units "K") 1024)
                                            ((equal units "M") (* 1024 1024))
                                            ((equal units "G") (* 1024 1024 1024))
                                            (t 1))))))))

;; http://en.wikipedia.org/wiki/Escape_sequences_in_C
(defparameter *escape-chars*
  '((#\a 7)
    (#\b 8)
    (#\e 27)
    (#\f 12)
    (#\n 10)
    (#\r 13)
    (#\t 9)))

(defun is-escape-char (ch) (assoc ch *escape-chars*))

(defrule string-contents-common-escape (and #\\ character)
  (:lambda (list)
    (make-instance 'string-node :data (if (is-escape-char (second list))
                                          (text (code-char (second (assoc (second list) *escape-chars*))))
                                          (text (second list))))))

(define-symbol-macro %code-char (make-instance 'string-node :data (text (code-char (parse-integer (text (cddr list)) :radix 16)))))
(defrule string-contents-common-x (and #\\ #\x hex-digit hex-digit) (:lambda (list) %code-char))
(defrule string-contents-common-u (and #\\ #\u hex-digit hex-digit hex-digit hex-digit) (:lambda (list) %code-char))
(defrule string-contents-common-cap-u (and #\\ #\U
                                           hex-digit hex-digit hex-digit hex-digit
                                           hex-digit hex-digit hex-digit hex-digit) (:lambda (list) %code-char))

(defrule string-contents-common (or
                                 string-contents-common-x
                                 string-contents-common-u
                                 string-contents-common-cap-u
                                 string-contents-common-escape))

(defrule string-contents-var (and "$" varname)
  (:lambda (list)
    ;; (format t "string-contents-var: ~S~%" list)
    (second list)))

(defrule string-contents-expression (and "${" optional-space expression optional-space "}")
  (:lambda (list)
    (third list)))

(defmacro def-string-rules (rule-name start-char end-char &body contents-rules)
  (let ((not-ending-name (intern (concatenate 'string (symbol-name rule-name) "-NOT-ENDING-CHAR")))
        (contents-name (intern (concatenate 'string (symbol-name rule-name) "-CONTENTS"))))
    `(progn
       (defun ,not-ending-name (x) (not (eq x ,end-char)))
       (defrule ,not-ending-name (,not-ending-name character) (:lambda (list) (make-instance 'string-node :data (text list))))
       (defrule ,contents-name (+ (or ,@contents-rules ,not-ending-name)))
       (defrule ,rule-name (and ,start-char (* ,contents-name) ,end-char)
         (:lambda (list &bounds start end) (make-instance 'string-container-node :children (first (second list)) :src %std-src))))))

(def-string-rules string-sq #\' #\' string-contents-common)
(def-string-rules string-dq #\" #\" string-contents-var string-contents-expression string-contents-common)
(def-string-rules string-regexp #\/ #\/ string-contents-var string-contents-expression string-contents-common)

(defrule string (or string-sq string-dq))

(defrule literal (and identifier string)
  (:lambda (list &bounds start end) (make-instance 'literal-node :children list :src %std-src)))

(defmacro def-regexp-rule ()
  `(defrule regexp (and string-regexp (* (or ,@(mapcar #'first *regexp-flags*))))
     (:lambda (list &bounds start end)
       (make-instance 'regexp-node
                      :data (text (second list))
                      :children (list (first list))
                      :src %std-src))))

(def-regexp-rule)

(defrule letters (character-ranges (#\a #\z) (#\A #\Z)) (:lambda (list) (text list)))

(defrule identifier-first (+ (or letters "_")))

(defrule identifier-rest (* (or identifier-first digits)))

(defrule identifier-whole-text (and identifier-first identifier-rest)
  (:lambda (list) (text list)))

(defrule identifier-immediate identifier-whole-text
  (:lambda (list &bounds start end)
    (make-instance 'string-node :data list :src %std-src)))

(defrule identifier (or identifier-immediate string string-contents-var string-contents-expression))

(defrule setitem (and binary-expression-1 optional-space "[" optional-space expression optional-space "]" optional-space "=" optional-space expression)
  (:lambda (list &bounds start end)
    (make-instance 'setitem-node :children (list (first list) (fifth list) (nth 10 list)) :src %std-src)))

(defrule setattr (and binary-expression-1 "." identifier optional-space "=" optional-space expression)
  (:lambda (list &bounds start end)
    (make-instance 'setattr-node :children (list (first list) (third list) (seventh list)) :src %std-src)))

(defrule expression (or comment end function-definition lambda setitem setattr binary-expression-1))

(defrule varname identifier-whole-text
  (:lambda (list &bounds start end)
    ;; (format t "varname @ ~S~%" start)
    (make-instance 'varname-node :data list :src %std-src)))

(defun %bin-expr (n)
  (intern (concatenate 'string "BINARY-EXPRESSION-" (write-to-string n))))

;; Overall (ngs-compile) is faster (414ms -> 306ms in pre-compiled, 425ms -> 317ms in regular) as opposed
;; http://en.wikipedia.org/wiki/Operator-precedence_parser
;; Consing: 150M -> 85M
(defmacro define-binary-operations-rules ()
  `(defrule binary-expression-1 (and non-binary-operation (* (and (or ,@*binary-operators*) binary-expression-1)))
     (:lambda (list)
       (if (second list)
           (let ((operator (second (first (first (second list)))))
                 (other (second (first (second list)))))
             (if (and
                  (typep other 'binary-operation-node)
                  (>= (gethash operator *binary-operators-precedence*)
                      (gethash (node-data other) *binary-operators-precedence*)))
                 (let ((new-node (make-instance 'binary-operation-node
                                                :data operator
                                                :children (list (first list) (first (node-children other))))))
                   (setf (nth 0 (node-children other)) new-node)
                   other)
                 (make-instance 'binary-operation-node
                                :data operator
                                :children (list (first list) other))))
           (first list)))))

(defmacro define-binary-vars-rule ()
  `(defrule binary-var (and "(" (or ,@*binary-functions*) ")")
     (:lambda (list &bounds start end)
       (make-instance 'varname-node :data (second list) :src %std-src))))

(define-binary-vars-rule)

(defrule expressions (and expression (* (+ (and expressions-delimiter expression))) optional-space (* ";"))
  (:lambda (list)
    (make-instance 'expressions-node :children (append (list (first list)) (mapcar #'second (caadr list))))))

(defrule expressions-delimiter (or newline-space (and (? inline-space) ";" (? space))) (:constant nil))

(defmacro defrule-spaced-seq (name seq &body body)
  `(defrule
       ,name
       (and ,@(butlast (loop
                          for item in seq
                          for i from 0
                          collecting item
                          if (not (eq 'space (nth (1+ i) seq))) collecting 'optional-space)))
     ,@body))


(defrule function-parameter (and
                             (? (or "**" "*"))
                             identifier
                             optional-space
                             (? (and optional-space ":" optional-space varname))
                             (? (and optional-space "=" optional-space expression)))
  (:lambda (list)
    (make-instance 'function-parameter-node
                   :data (list
                          (cond ((equal "*"  (first list)) 'positional-rest)
                                ((equal "**" (first list)) 'named-rest)
                                (t 'regular)))
                   :children (list
                              (second list)
                              (fourth (fourth list))
                              (fourth (fifth list))))))

(defrule function-parameters (? (and function-parameter (* (and optional-space "," optional-space function-parameter optional-space))))
  (:lambda (list)
    ;; (format t "~%WTF1: ~S~%" (make-instance 'function-parameters-node :children (append (list (first list)) (mapcar #'fourth (cadr list)))))
    (make-instance 'function-parameters-node :children (when list (append (list (first list)) (mapcar #'fourth (cadr list)))))))


(defrule-spaced-seq function-parameters-with-parens ("(" function-parameters ")") (:lambda (list) (third list)))

(defrule-spaced-seq function-definition ((or "defg" "def") space identifier function-parameters-with-parens "{" (? expressions) "}")
  (:lambda (list)
    (make-instance 'function-definition-node
                   :data (list (equal "defg" (first list)))
                   :children (list
                              (fourth list)
                              (sixth list)
                              (tenth list)))))

(defmacro define-lambda (name &body names)
  `(defrule ,name (and (or ,@names) (? (and space identifier)) optional-space function-parameters-with-parens optional-space "{" optional-space (? expressions) optional-space "}")
    (:lambda (list)
      (make-instance 'lambda-node
                     :children (list
                                (second (second list))
                                (fourth list)
                                (eighth list))))))

(define-lambda lambda "F" "lambda")
(define-lambda catch "catch")

(define-binary-operations-rules)

(defrule assignment (and (? (and "global" space)) identifier optional-space "=" optional-space expression)
  (:lambda (list)
    (make-instance 'assignment-node
                   :data (not (not (first list)))
                   :children (list (second list) (sixth list)))))

;; TODO: named arguments
(defrule function-argument (or
                            (and identifier optional-space "=" optional-space expression)
                            expression)
  (:lambda (list)
    ;; (format t "ARG: ~S~%" list)
    (make-instance 'function-argument-node :children (list list))))


;; TODO: named arguments
(defrule function-arguments (? (and function-argument (* (and optional-space "," optional-space function-argument optional-space))))
  (:lambda (list)
    ;; (format t "ARGS: ~S~%" list)
    (make-instance 'function-arguments-node :children (when list (append (list (first list)) (mapcar #'fourth (cadr list)))))))

(defrule-spaced-seq function-call (expression "(" function-arguments ")")
  (:lambda (list &bounds start end)
    (make-instance 'function-call-node :children (list (first list) (fifth list)) :src %std-src)))

(defrule true  "true"  (:constant (make-instance 'keyword-node :data :true)))
(defrule false "false" (:constant (make-instance 'keyword-node :data :false)))
(defrule null  "null"  (:constant (make-instance 'keyword-node :data :null)))

(defrule list (and "[" optional-space (? (and expression (* (and optional-space "," optional-space expression optional-space)))) optional-space "]")
  (:lambda (list &bounds start end)
    (process-possible-splice
     'array-concat-node
     (make-instance 'array-node
                    :children (when (first (third list)) (append (list (first (third list))) (mapcar #'fourth (second (third list)))))
                    :src %std-src))))

(defrule splice (and "*" optional-space expression)
  (:lambda (list &bounds start end)
    (make-instance 'splice-node
                   :children (list (third list))
                   :src %std-src)))

(defrule kv-pair (and expression optional-space ":" optional-space expression)
  (:lambda (list &bounds start end)
    (make-instance 'kv-pair-node
                   :children (list (first list) (fifth list))
                   :src %std-src)))

(defrule kv-splice (and "**" optional-space expression)
  (:lambda (list &bounds start end)
    (make-instance 'kv-splice-node
                   :children (list (third list))
                   :src %std-src)))

(defrule hash-item (or kv-pair kv-splice))

(defrule hash (and "{" optional-space (? (and hash-item (* (and optional-space "," optional-space hash-item optional-space)))) optional-space "}")
  (:lambda (list &bounds start end)
     (make-instance 'hash-node
                    :children (when (first (third list)) (append (list (first (third list))) (mapcar #'fourth (second (third list)))))
                    :src %std-src)))


(defrule non-binary-operation (or chain))

(defrule chain (and non-chain (* chain-item))
  (:lambda (list)
    (let ((result (first list)))
      (loop for n in (second list)
         do (nsubst result :arg (second n))
         do (setq result (first n)))
      result)))

(defrule chain-item (or
                     chain-item-dot-call
                     chain-item-getattr
                     chain-item-getitem))

(defrule chain-item-dot-call (and "." varname optional-space "(" function-arguments ")")
  (:lambda (list &bounds start end)
    (let*
        ((args (fifth list))
         (target-list (list :arg)))
      (setf (node-children args) (cons (make-instance 'function-argument-node :children target-list) (node-children args)))
      (list
       (make-instance 'function-call-node :children (list (second list) args) :src %std-src)
       target-list))))

(defrule chain-item-getattr (and "." identifier (! (and optional-space "=" (! "="))))
  (:lambda (list &bounds start end)
    (let ((target-list (list :arg (second list))))
      (list
       (make-instance 'getattr-node :children target-list :src %std-src)
       target-list))))

(defrule chain-item-getitem (and optional-space "[" optional-space expression optional-space "]" (! (and optional-space "=" (! "="))))
  (:lambda (list &bounds start end)
    (let ((target-list (list :arg (fourth list))))
      (list
       (make-instance 'getitem-node :children target-list :src %std-src)
       target-list))))

(defrule curly-braces-expressions (and "{" optional-space expressions optional-space "}") (:lambda (list) (third list)))

(defrule if (and "if" space expression optional-space curly-braces-expressions (? (and optional-space (? (and "else" optional-space)) curly-braces-expressions)))
  (:lambda (list &bounds start end)
    (make-instance 'if-node
                   :children (list
                              (third list)
                              (fifth list)
                              (if (sixth list) (third (sixth list)) (make-instance 'keyword-node :data :null)))
                   :src %std-src)))

(defrule try-catch (and "try" optional-space curly-braces-expressions (+ (and optional-space catch)))
  (:lambda (list &bounds start end)
    (make-instance 'try-catch-node :children (apply #'list (third list) (mapcar #'second (fourth list))) :src %std-src)))

(defrule throw (and "throw" space expression)
  (:lambda (list &bounds start end)
    (make-instance 'throw-node :children (list (third list)) :src %std-src)))

(defun items-at-positions (positions list)
  (loop for p in positions collecting (nth p list)))

(defrule-spaced-seq for ("for" "(" expression ";" expression ";" expression ")" curly-braces-expressions)
  (:lambda (list &bounds start end)
    (make-instance 'for-node
                   :children (items-at-positions '(4 8 12 16) list)
                   :src %std-src)))

(defrule while (and "while" space expression optional-space curly-braces-expressions)
  (:lambda (list &bounds start end)
    (make-instance 'while-node
                   :children (items-at-positions '(2 4) list)
                   :src %std-src)))

(defrule guard (and "guard" space expression)
  (:lambda (list &bounds start end)
    (make-instance 'guard-node :children (list (third list)) :src %std-src)))

(defrule return (and "return" space expression)
  (:lambda (list &bounds start end)
    (make-instance 'return-node :children (list (third list)) :src %std-src)))

(defrule parentheses (and "(" optional-space expression optional-space ")")
  (:lambda (list) (third list)))

(defrule command-inline-spearator (and optional-space ";" optional-space))

(defrule command-separator (or newline-space command-inline-spearator))

(defrule command-word-text (+ (character-ranges #\- #\_ (#\a #\z) (#\A #\Z) (#\0 #\9) #\* #\? #\/))
  (:lambda (list &bounds start end)
    (make-instance 'string-node :data (text list) :src %std-src)))

(defrule command-expression (and "${" expression "}") (:lambda (list) (second list)))

(defrule command-splice-var (and "$*" varname)
  (:lambda (list &bounds start end)
    (make-instance 'splice-node
                   :children (list (second list))
                   :src %std-src)))

(defrule command-var (and "$" varname) (:lambda (list) (second list)))

(defrule command-word (or
                       command-expression
                       command-splice-var
                       command-var
                       command-word-text
                       string))

(defrule command (and command-word (* (and inline-space command-word)))
  (:lambda (list &bounds start end)
    (make-instance
     'command-node
     :children (apply #'list (first list) (mapcar #'second (second list)))
     :src %std-src)))

(defrule commands (and command (* (and command-separator command)))
  (:lambda (list &bounds start end)
    (make-instance
     'commands-node
     :data "$()" ; default for script top-level commands
     :children (apply #'list (first list) (mapcar #'second (second list)))
     :src %std-src)))

(defun %with-data (node data)
  (setf (node-data node) data)
  node)

(defrule spawn-commands-dollar-paren (and "$(" optional-space commands optional-space ")")
  (:lambda (list) (%with-data (third list) "$()")))
(defrule spawn-commands-backtick-backtick (and "``" optional-space commands optional-space "``")
  (:lambda (list) (%with-data (third list) "````")))
(defrule spawn-commands-backtick (and "`" optional-space commands optional-space "`")
  (:lambda (list) (%with-data (third list) "``")))

(defrule spawn-commands (or spawn-commands-dollar-paren spawn-commands-backtick-backtick spawn-commands-backtick))

(defrule at-lambda (and "@" optional-space expression) (:lambda (list) (%make-xyz-lambda-node (third list))))

(defrule non-chain (or
                    if
                    try-catch
                    throw
                    for
                    while
                    guard
                    return
                    assignment
                    function-call
                    number
                    literal
                    regexp
                    string
                    true
                    false
                    null
                    list
                    hash
                    splice
                    at-lambda
                    varname
                    spawn-commands
                    binary-var
                    parentheses))

(defrule code (and "{" optional-space expressions optional-space "}")
  (:lambda (list &bounds start end)
    (make-instance 'command-code-node :children (list (third list)) :src %std-src)))

(defrule top-level-item (or
                         comment
                         function-definition
                         function-call
                         assignment
                         code
                         end
                         commands))

(defrule top-level (and top-level-item (* (and command-separator top-level-item)) optional-space)
  (:lambda (list)
    ;; (format t "~S~%" (first (second list)))
    (make-instance 'expressions-node
                   :children (apply #'list (first list) (mapcar #'second (second list))))))


;; Parser - end ------------------------------

;; Variables - start ------------------------------

(defclass lexical-scopes ()
  ((hashes
    :initform (list (make-hash-table :test #'equal :size 100))
    :initarg :hashes
    :accessor lexical-scopes-hashes)))

(defmethod print-object ((ls lexical-scopes) stream)
  (format stream "#<LEXICAL-SCOPES ~S>"
          (loop
             for hash in (lexical-scopes-hashes ls)
             collecting
               (loop
                  for key being the hash-keys of hash
                  collecting (list key (gethash key hash))))))

(defvar *ngs-globals* (make-instance 'lexical-scopes))
(defvar *source-position* nil)
;; TODO: consider :synchronized
(defvar *ngs-meta* (make-hash-table :weakness :key))
(defvar *ngs-objects-types* (make-hash-table :weakness :key))
(defvar *ngs-objects-attributes* (make-hash-table :weakness :key))

(defun one-level-deeper-lexical-vars (ls)
  (make-instance 'lexical-scopes :hashes (cons (make-hash-table :test #'equal :size 20) (lexical-scopes-hashes ls))))

(defun get-var (name vars &optional (include-top-level t))
  ;; (format t "GET ~S~%" name)
  (loop for hash in (if include-top-level
                        (lexical-scopes-hashes vars)
                        (butlast (lexical-scopes-hashes vars)))
     do (multiple-value-bind (result found) (gethash name hash)
          (when found (return-from get-var (values result hash)))))
  (error 'variable-not-found :varname name :stack-trace *source-position*))

(defun get-var-or-default (name vars default)
  (handler-case (get-var name vars)
    (variable-not-found () default)))

(defun set-var (name vars value &optional (global t))
  ;; (format t "SET ~S=~S~%" name value)
  (let ((dst-hash
         (handler-case (nth-value 1 (get-var name vars global))
           (variable-not-found ()
             (if global
                 (first (last (lexical-scopes-hashes vars)))
                 (first (lexical-scopes-hashes vars)))))))
    (setf (gethash name dst-hash) value)))

(defun set-local-var (name vars value) (set-var name vars value nil))

(defun %set-global-variable (name value)
  ;; (format t "X ~S ~%" (last (lexical-scopes-hashes *ngs-globals*)))
  (setf (gethash name (first (lexical-scopes-hashes *ngs-globals*))) value))

;; Variables - end ------------------------------


;; Types definitions - start ------------------------------

(defstruct ngs-type name parents constructors predicate)
(defclass ngs-object ()
  ((type
    :initarg :type
    :accessor ngs-object-type)
   (attributes
    :initform (make-hash-table :test #'equal :size 8)
    :initarg :attributes
    :accessor ngs-object-attributes)))


(defmethod print-object ((typ ngs-type) stream)
  (format stream "#<ngs-type ~A>" (ngs-type-name typ)))

(defmethod print-object ((o ngs-object) stream)
  (format stream "#<ngs-object ~A" (ngs-type-name (ngs-object-type o)))
  (let ((h (ngs-object-attributes o)))
    (loop
       for key being the hash-keys of h
       do (format stream " ~A=~A" key (gethash key h))))
  (format stream ">"))


(defun %ngs-type-symbol (type-name)
  (intern (concatenate 'string "NGS-TYPE-" (string-upcase type-name))))

(defmacro def-ngs-type (name predicate)
  (let*
      ((symb (%ngs-type-symbol name)))
    `(progn
       (defvar ,symb
         (make-ngs-type :name ,name :predicate ,predicate))
       (%set-global-variable ,name ,symb))))

;; (def-ngs-type "Any"    #'(lambda (x) (declare (ignore x)) t))
(def-ngs-type "Array"  #'(lambda (x) (and (arrayp x) (not (stringp x)))))
(def-ngs-type "Bool"   #'(lambda (x) (or (eq x :true) (eq x :false))))
(def-ngs-type "F"      #'(lambda (x) (or
                                      (functionp x)
                                      (ngs-type-p x)
                                      (and (listp x) (functionp (first x))))))
(def-ngs-type "File"    #'pathnamep)
(def-ngs-type "Hash"    #'hash-table-p)
(def-ngs-type "List"    #'listp)
(def-ngs-type "Null"    #'(lambda (x) (eq x :null)))
(def-ngs-type "Number"  #'numberp)
(def-ngs-type "Process" #'sb-ext:process-p)
(def-ngs-type "Regexp"  #'(lambda (x) (eq :regexp (gethash x *ngs-objects-types*))))
(def-ngs-type "Seq"     #'(lambda (x) (or (arrayp x) (listp x)))) ; arrayp includes stringp, arrayp has higher probability
(def-ngs-type "Stream"  #'streamp)
(def-ngs-type "String"  #'stringp)
(def-ngs-type "Type"    #'ngs-type-p)

;; Types definitions - end ------------------------------

;; Compiler - start ------------------------------

(defgeneric generate-code (node))

(define-symbol-macro %1 (generate-code (first (node-children n))))
(define-symbol-macro %2 (generate-code (second (node-children n))))
(define-symbol-macro %3 (generate-code (third (node-children n))))
(define-symbol-macro %4 (generate-code (fourth (node-children n))))
(define-symbol-macro %children (children-code n))
(define-symbol-macro %data (node-data n))

(defun children-code (node &key (start 0))
  (mapcar #'generate-code
          (remove-if #'(lambda (x) (typep x 'incompilable-node))
                     (subseq (node-children node) start))))

(defun generate-expected-parameters (n)
  `(list
    ,@(mapcar
       #'(lambda(x) `(list ,@(apply #'list (mapcar #'generate-code (node-children x)))))
       (node-children n))))

;; TODO :src
(defun string-container-children-optimize (list)
  (if list
      (let ((p (position-if-not #'(lambda (x) (typep x 'string-node)) list)))
        (if (eq 0 p)
            (cons
             (first list)
             (string-container-children-optimize (rest list)))
            (cons
             (make-instance 'string-node :data (apply #'concatenate 'string (mapcar #'(lambda (x) (node-data x)) (subseq list 0 p))))
             (subseq list (or p (length list))))))
      nil))

(defun make-function-call-node (fname positionals)
  (make-instance
   'function-call-node
   :children (list
              (make-instance 'varname-node :data fname)
              (make-instance 'function-arguments-node :children (mapcar #'(lambda (x) (make-instance 'function-argument-node :children (list x))) positionals)))
   :src (node-src (first positionals))))

(defun wrap-with-call-to-string (n)
  (if (typep n 'string-node)
      n
      (make-function-call-node "String" (list n))))

;; For simplicity of generate-expected-parameters, which has nullable fields in each parameter

(defmacro %bool-ecase (expr yes no)
  `(ecase
       (ngs-call-function (get-var "Bool" vars) (make-arguments :positional (list ,expr)) :name "Bool")
     (:true ,yes)
     (:false ,no)))


(defun %make-function-parameter-node (name)
  (make-instance
   'function-parameter-node
   :data (list 'regular)
   :children (list
              (make-instance 'string-node :data name)
              nil
              (make-instance 'keyword-node :data :null))))

(defun %make-xyz-lambda-node (expr)
  (make-instance
   'lambda-node
   :src (node-src expr)
   :children (list
              "auto-generated-@-or-@?"
              (make-instance
               'function-parameters-node
               :children (list
                          (%make-function-parameter-node "X")
                          (%make-function-parameter-node "Y")
                          (%make-function-parameter-node "Z")))
              expr)))

(defmethod generate-code ((n null))                      nil)

(defmethod generate-code ((n number-node))               %data)
(defmethod generate-code ((n string-node))               %data)
(defmethod generate-code ((n varname-node))             `(get-var ,%data vars))
(defmethod generate-code ((n binary-operation-node))    (let ((op %data))
                                                          (cond
                                                            ((equal op "and") `(let ((r ,%1)) (%bool-ecase r ,%2 r)))
                                                            ((equal op "or") `(let ((r ,%1)) (%bool-ecase r r ,%2)))
                                                            ((equal op "@")
                                                             ;; Look at (parse 'expression "F(X=null,Y=null,Z=null) {1}")
                                                             (generate-code
                                                              (make-function-call-node
                                                               "map"
                                                               (list
                                                                (first (node-children n))
                                                                (%make-xyz-lambda-node (second (node-children n)))))))
                                                            ((equal op "@?")
                                                             (generate-code
                                                              (make-function-call-node
                                                               "filter"
                                                               (list
                                                                (first (node-children n))
                                                                (%make-xyz-lambda-node (second (node-children n)))))))
                                                            ((equal op "returns")
                                                             ;; Look at (parse 'expression "F(X=null,Y=null,Z=null) {1}")
                                                             `(%bool-ecase
                                                                             ,%1
                                                                             (return-from function-block ,%2)
                                                                             :null))
                                                            (t
                                                             `(ngs-call-function (get-var ,op vars)
                                                                                 (make-arguments :positional (list ,@(children-code n)))
                                                                                 :name ,op)))))
(defmethod generate-code ((n assignment-node))          `(set-var
                                                          ,(node-data (first (node-children n)))
                                                          vars
                                                          ,@(children-code n :start 1)
                                                          ,(node-data n)))
(defmethod generate-code ((n expressions-node))         (let ((c %children))
                                                          (if c `(progn ,@%children) :null)))

(defmethod generate-code ((n function-definition-node))
  ;; `(let ((expected-parameters ,(generate-expected-parameters (second (node-children n)))))
  (let ((ret `(ngs-define-function
           ,%1
           vars
           ,(first %data)
           ;; expected-parameters
           (lambda (parameters)
             (block function-block
               (let ((vars (one-level-deeper-lexical-vars vars)))
                 (declare (ignorable vars))
                 ,@(children-code n :start 1)))))))
    ;; (format t "sec: ~S~%" (first (node-children (second (node-children n)))))
    (if (first (node-children (second (node-children n))))
        `(let ((expected-parameters ,(generate-expected-parameters (second (node-children n))))) ,ret)
        ret)))

(defmethod generate-code ((n lambda-node))              `(let ((expected-parameters ,(generate-expected-parameters (second (node-children n)))))
                                                           (lambda (parameters)
                                                             (block function-block
                                                              (let ((vars (one-level-deeper-lexical-vars vars)))
                                                                ,@(children-code n :start 1))))))

;; 1. match the parameters and signal if there is a mismatch
;; 2. set local variables
;; 3. do it smarter and more efficient
(defmethod generate-code ((n function-parameters-node)) `(progn
                                                           ,(when
                                                             (notany
                                                              #'(lambda (p) (eq (first (node-data p)) 'positional-rest))
                                                              (node-children n))
                                                             `(when (> (length (arguments-positional parameters)) ,(length (node-children n)))
                                                                (error 'parameters-mismatch)))
                                                           ,@(loop
                                                                for p in (node-children n)
                                                                for pc = (node-children p)
                                                                for i from 0
                                                                collecting
                                                                  (cond
                                                                    ((eq (first (node-data p)) 'positional-rest)
                                                                     `(set-local-var
                                                                       (first (nth ,i expected-parameters))
                                                                       vars
                                                                       (subseq (arguments-positional parameters) ,i)))
                                                                    (t
                                                                     `(set-local-var (first (nth ,i expected-parameters)) vars
                                                                                     (if
                                                                                      (> (length (arguments-positional parameters)) ,i)
                                                                                      (guard-type
                                                                                       (nth ,i (arguments-positional parameters))
                                                                                       (second (nth ,i expected-parameters)))
                                                                                      ,(if (third pc)
                                                                                           `(third (nth ,i expected-parameters))
                                                                                           `(error 'parameters-mismatch)))))))))

(defmethod generate-code ((n function-call-node))       (if
                                                         (typep (first (node-children n)) 'varname-node)
                                                         `(ngs-call-function ,%1 ,%2 :name ,(node-data (first (node-children n))))
                                                         `(ngs-call-function ,%1 ,%2)))


;; (defmethod generate-code :before ((n function-call-node)) (format t "First: ~S~%" (typep (first (node-children n)) 'varname-node)))

;; TODO: support named arguments
(defmethod generate-code ((n function-arguments-node))  `(make-arguments
                                                          :positional
                                                          ,(generate-code
                                                            (process-possible-splice
                                                             'list-concat-node
                                                             (make-instance 'list-node
                                                                            :children (mapcar #'(lambda (a) (first (node-children a))) (node-children n)))))))

(defmethod generate-code ((n keyword-node))             %data)
(defmethod generate-code ((n list-node))                `(list ,@%children))
(defmethod generate-code ((n array-node))               `(make-array
                                                          ,(length (node-children n))
                                                          :adjustable t
                                                          :fill-pointer t
                                                          :initial-contents (list ,@%children)))

(defmethod generate-code ((n kv-pair-node))             `(setf (gethash ,%1 h) ,%2))

(defmethod generate-code ((n kv-splice-node))           `(let ((src-hash
                                                                (ngs-call-function (get-var "Hash" vars)
                                                                                   (make-arguments :positional (list ,%1))
                                                                                   :name "Hash")))
                                                           (loop
                                                              for key being the hash-keys of src-hash
                                                              do (setf (gethash key h) (gethash key src-hash)))))

(defmethod generate-code ((n hash-node))                `(let ((h (make-hash-table :test #'equal)))
                                                           ,@%children
                                                           h))

(defmethod generate-code ((n list-concat-node))         `(concatenate 'list ,@%children))

(defmethod generate-code ((n array-concat-node))        `(let ((l (concatenate 'list ,@%children)))
                                                           (make-array
                                                            (length l)
                                                            :adjustable t
                                                            :fill-pointer t
                                                            :initial-contents l)))

(defmethod generate-code ((n string-container-node))    (if (null (node-children n))
                                                            ""
                                                            (let* ((children (string-container-children-optimize (node-children n)))
                                                                   (stringified-children (mapcar #'wrap-with-call-to-string children)))
                                                              (if (eq 1 (length stringified-children))
                                                                  (generate-code (first stringified-children))
                                                                  `(apply #'concatenate (list 'string ,@(mapcar #'generate-code stringified-children)))))))

(defmethod generate-code ((n getattr-node))             `(ngs-call-function
                                                          (get-var "." vars)
                                                          (make-arguments :positional (list ,@(children-code n)))
                                                          :name "."))

(defmethod generate-code ((n getitem-node))             `(ngs-call-function
                                                          (get-var "[]" vars)
                                                          (make-arguments :positional (list ,@(children-code n)))
                                                          :name "[]"))

(defmethod generate-code ((n setitem-node))             `(ngs-call-function
                                                          (get-var "[]=" vars)
                                                          (make-arguments :positional (list ,@(children-code n)))
                                                          :name "[]="))

(defmethod generate-code ((n setattr-node))             `(ngs-call-function
                                                          (get-var ".=" vars)
                                                          (make-arguments :positional (list ,@(children-code n)))
                                                          :name ".="))

(defmethod generate-code ((n if-node))                  `(%bool-ecase ,%1 ,%2, %3))
(defmethod generate-code ((n try-catch-node))           `(handler-case ,%1
                                                           (ngs-user-exception (e)
                                                             (ngs-call-function
                                                              (list ,@(children-code n :start 1))
                                                              (make-arguments :positional (list (ngs-user-exception-datum e)))))))

(defmethod generate-code ((n throw-node))               `(error 'ngs-user-exception :datum ,%1))

(defmethod generate-code ((n guard-node))               `(%bool-ecase ,%1 :null (error 'parameters-mismatch)))

(defmethod generate-code ((n return-node))              `(return-from function-block ,%1))

(defmethod generate-code ((n for-node))                 `(progn
                                                           ,%1
                                                           (loop while (%bool-ecase ,%2 t nil) do ,%4 do ,%3)
                                                           :null))

(defmethod generate-code ((n while-node))               `(progn (loop while (%bool-ecase ,%1 t nil) do ,%2) :null))

(defmethod generate-code ((n literal-node))             `(ngs-call-function
                                                          (get-var (apply #'concatenate 'string (list "__literal_" ,%1)) vars)
                                                          (make-arguments :positional (list ,%2))))

(defmethod generate-code ((n regexp-node))              `(ngs-call-function (get-var "Regexp" vars) (make-arguments :positional (list ,%1 ,(node-data n))) :name "Regexp"))

(defmethod generate-code ((n command-node))             `(ngs-call-function
                                                          spawn-function
                                                          (make-arguments
                                                           :positional
                                                           (list
                                                            (ngs-call-function
                                                             (get-var "Command" vars)
                                                             (make-arguments
                                                              :positional
                                                              ,(generate-code
                                                                (process-possible-splice
                                                                 'list-concat-node
                                                                 (make-instance
                                                                  'list-node
                                                                  :children
                                                                  (node-children n)))))
                                                             :name "Command")))
                                                          :name spawn-function-name))

(defmethod generate-code ((n command-code-node))        (generate-code (first (node-children n))))
(defmethod generate-code ((n commands-node))            `(let ((spawn-function (get-var ,%data vars))
                                                               (spawn-function-name ,%data)) ,@%children))


;; GENERATE MARKER

;; Total running time differences for stdlib2.ngs
;; 2.28 -> 1.30 sec for compiled
;; 4.12 -> 2.81 sec for regular
(when (null (sb-posix:getenv "NGS_NO_SOURCE_POS"))
  (defmethod generate-code :around ((n node))
             (if (node-src n)
                 `(let ((*source-position* (cons ,(or (node-src n) "<unknown>") *source-position*)))
                    ,(call-next-method))
                 (call-next-method))))

(defun make-source-file-positions (code)
  "Positions where lines start"
  (apply #'vector 0 (loop
                     for char across code
                     for position from 0
                     if (eq #\Newline char) collecting (1+ position))))

(defun ngs-compile (code file-name)
  (let* ((*source-file-name* file-name)
         (*source-file-positions* (make-source-file-positions code))
         (c (generate-code (parse 'top-level code))))
    `(let ((vars *ngs-globals*))
       (declare (ignorable vars))
       (handler-case ,c
         (runtime-error (e) (format t "Run-time error: ~A~%Stack: ~S" e (runtime-error-stack-trace e)))))))

;; Compiler - end ------------------------------

;; Runtime - start ------------------------------

(define-condition runtime-error () ((stack-trace :initarg :stack-trace :initform nil :reader runtime-error-stack-trace)))
(define-condition variable-not-found (runtime-error) ((varname :initarg :varname :reader variable-not-found-varname)))
(define-condition method-implementatoin-not-found (runtime-error)
  ((method-name :initarg :method-name :reader method-implementatoin-not-found-method-name)
   (arguments :initarg :arguments :reader method-implementatoin-not-found-arguments)))
(define-condition calling-non-a-method (runtime-error) ())
(define-condition parameters-mismatch () ())
(define-condition ngs-user-exception (runtime-error) ((datum :initarg :datum :reader ngs-user-exception-datum)))
(define-condition item-does-not-exist (runtime-error) ((datum :initarg :datum :reader ngs-user-exception-datum)))
(define-condition attribute-does-not-exist (runtime-error) ((datum :initarg :datum :reader ngs-user-exception-datum)))

(defmethod print-object ((e variable-not-found) stream)
  (format stream "Variable '~A' not found" (variable-not-found-varname e)))

(defmethod print-object ((e item-does-not-exist) stream)
  (format stream "Item '~A' not found" (ngs-user-exception-datum e)))

(defmethod print-object ((e ngs-user-exception) stream)
  (format stream "User exception: ~A" (ngs-user-exception-datum e)))

(defmethod print-object ((e method-implementatoin-not-found) stream)
  (let ((name (method-implementatoin-not-found-method-name e)))
    (if name
        (format stream "Method implementation '~A' not found." name)
        (format stream "Method implementation with unknown name (probably computed) not found."))
    (format stream "Arguments: ~S" (method-implementatoin-not-found-arguments e))))

;; TODO: check parents
;; TODO: don't call with nil as typ, eliminate the calls instead, remove (or) here
(defun ngs-value-is-of-type (val typ)
  (or (null typ) (funcall (ngs-type-predicate typ) val)))

(defun guard-type (val typ)
  (unless (ngs-value-is-of-type val typ)
    (error 'parameters-mismatch))
  val)

(defun ngs-user-type-is (checked-type super-type)
  (or
   (eq checked-type super-type)
   (loop
      for parent-type in (ngs-type-parents checked-type)
      ;; do (format t "[c ~S]" super-type)
      if (ngs-user-type-is parent-type super-type) return t)))

;; TODO: later, optimize by recompiling super-types after inherits() and
;;       embedding all possible children immediately in the predicate
(defun make-user-defined-type-predicate (typ)
  (lambda (x) (and (typep x 'ngs-object) (ngs-user-type-is (ngs-object-type x) typ))))

(defun hash-keys (h)
  (loop for key being the hash-keys of h
     collecting key))

;; XXX - some issues, probably global/local
(defun ngs-define-function (function-name vars global lambda)
  (let ((v (get-var-or-default function-name vars nil)))
    (if (typep v 'ngs-type)
        (setf (ngs-type-constructors v) (cons lambda (ngs-type-constructors v)))
        (set-var function-name vars (cons lambda v) global))))

;; TODO - handle parameters-mismatch
(defun ngs-call-function (methods arguments &key (name nil))
  ;; (format t "METHODS: ~S~%" methods)
  (cond
    ((typep methods 'ngs-type) (ngs-call-function (ngs-type-constructors methods) arguments :name name))
    ((functionp methods) (ngs-call-function (list methods) arguments :name name))
    ((listp methods)
     ;; TODO: check that at least the first element is callable
     (progn
       (loop for m in methods
          ;; do (format t "+ Trying implementation ~A~%" m)
          do (handler-case (return-from ngs-call-function (funcall m arguments))
               (parameters-mismatch () nil)))
       ;; (format t "METHODS WERE: ~S~%" methods)
       (error 'method-implementatoin-not-found :stack-trace *source-position* :method-name name :arguments arguments)))
    (t
     (error 'calling-non-a-method))))

;; (handler-case (get-var name vars)
;;     (variable-not-found () default)))

(define-symbol-macro %positionals (arguments-positional parameters))
(define-symbol-macro %p1 (first %positionals))
(define-symbol-macro %p2 (second %positionals))
(define-symbol-macro %p3 (third %positionals))

(defun find-in-tree (item tree)
  (if tree
      (or
       (eq (car tree) item)
       (and (listp (car tree)) (find-in-tree item (car tree)))
       (find-in-tree item (cdr tree)))
      nil))

(defmacro native (name params &body body)
  ;; (format t "find-in-tree: ~S~%" (find-in-tree '%p1 body))
  `(ngs-define-function
    ,name *ngs-globals*
    t
    (lambda (parameters)
      (when (not (eq (length %positionals) ,(length params)))
        (error 'parameters-mismatch))
      (let* ((*source-position* (cons ,(format nil "<builtin:~A>" name) *source-position*)))
        ,@(loop
             for p in params
             for i from 0
             if (not (eq 'any p))
             collecting `(guard-type (nth ,i (arguments-positional parameters)) ,(%ngs-type-symbol (symbol-name p))))
        ,@body))))

(defmacro all-positionals (typ)
  `(loop for p in %positionals do (guard-type p ,typ)))

;; TODO: %p2 is computed twice
(defmacro native-getattr (typ &body body) `(native "." (,typ string)
                                             (cond
                                               ,@(loop
                                                    for clause in body
                                                    collecting `((equal %p2 ,(first clause)) ,@(rest clause)))
                                               (t (error 'attribute-does-not-exist :datum %p2)))))

(defmacro %call (name parameters)
  `(ngs-call-function (get-var ,name *ngs-globals*) ,parameters :name ,name))

(defun %bool (x) (if x :true :false))
(defun %nil->nul (x) (if x x :null))

(defun %array (init) (make-array (length init) :adjustable t :fill-pointer t :initial-contents init))

(native "==" (any any) (%bool (equal %p1 %p2)))
(native "!=" (any any) (ecase (%call "==" (make-arguments :positional (list %p1 %p2)))
                               (:true :false)
                               (:false :true)))

(native "===" (any any) (%bool (eq %p1 %p2)))
(native "!==" (any any) (%bool (not (eq %p1 %p2))))
(native "<" (number number) (%bool (< %p1 %p2)))
(native ">" (number number) (%bool (> %p1 %p2)))
(native "+" (number number) (+ %p1 %p2))
(native "-" (number number) (- %p1 %p2))
(native "*" (number number) (* %p1 %p2))
(native "/" (number number) (/ %p1 %p2))
(native "+" (string string) (concatenate 'string %p1 %p2))

(native "globals" () (first (lexical-scopes-hashes *ngs-globals*)))

;; Type
(native "Type" (string)
  (let* ((type-name %p1)
         (type (make-ngs-type :name type-name)))
    (setf (ngs-type-predicate type) (make-user-defined-type-predicate type))
    ;; TODO: not ignore parameters
    (setf (ngs-type-constructors type) (list (lambda (parameters) (declare (ignore parameters)) (make-instance 'ngs-object :type type))))
    type))
(native "is" (any type) (%bool (ngs-value-is-of-type %p1 %p2)))
(native "obj" (type) (make-instance 'ngs-object :type %p1))
(native "inherits" (type type)
  (let ((dst-type %p1))
    (setf (ngs-type-parents dst-type) (cons %p2 (ngs-type-parents dst-type)))))
(native "as" (any type)
  (make-instance 'ngs-object :type %p2 :attributes (ngs-object-attributes %p1)))

(native-getattr type
  ("name" (ngs-type-name %p1))
  ("constructors" (ngs-type-constructors %p1)))

;; Bool
(native "Bool" (bool) %p1)
(native "Bool" (number) (%bool (not (zerop %p1))))
(native "Bool" (list) (%bool (not (null %p1)))) ; probably move to stdlib later
(native "Bool" (null) :false)

;; Number
(native "chr" (number) (coerce (list (code-char %p1)) 'string))

;; List
(native "[]" (list number) (nth %p2 %p1))
(native "in" (any list) (%bool (member %p1 %p2 :test #'equal)))

;; Array
(native "[]" (array number) (elt %p1 %p2))
(native "push" (array any) (vector-push-extend %p2 %p1) %p1)
(native "in" (any array) (%bool (position %p1 %p2))) ; probably move to stdlib later, provide (position)

;; Sequence
(native "len" (seq) (length %p1))
(native "slice" (seq number number) (let ((start %p2)) (subseq %p1 start (+ start %p3))))
(native "pos" (seq seq number) (or (search %p2 %p1 :start2 %p3) :null)) ; %p1 - seq, %p2 - subseq, %p3 - start in seq
(native "pos" (seq seq) (or (search %p2 %p1) :null)) ; %p1 - seq, %p2 - subseq

;; TODO: complete comparator-<
(defun comparator-< (a b)
  (cond
    ((and (stringp a) (stringp b)) (string< a b))
    ((and (numberp a) (numberp b)) (< a b))
    ((and (numberp a) (stringp b)) t)
    ((eq a :null) (not (eq b :null)))
    (t nil)))
    ;; ((eq a :false) (not (eq b :false)))
    ;; ((eq a :true) (not (eq b :true)))))

(native "sort" (seq) (sort %p1 #'comparator-<))

;; String
(native "String" (any) (format nil "~A" %p1))
;; (native "String" (file) (file-name %p1))
(native "[]" (string number) (let ((pos %p2)) (subseq %p1 pos (1+ pos))))
(native "in" (string string) (%bool (search %p1 %p2)))
(native "ord" (string) (char-code (elt %p1 0)))

;; Hash
(native "Hash" (hash) %p1)
(native "Hash" () (make-hash-table :test #'equal))
(native "[]" (hash any)
  (multiple-value-bind (result found) (gethash %p2 %p1)
    (if found
        result
        (error 'item-does-not-exist :datum %p2))))
(native "[]=" (hash any any) (setf (gethash %p2 %p1) %p3))
(native "in" (any hash) (%bool (nth-value 1 (gethash %p1 %p2))))
(native "keys" (hash)
  (let ((keys (hash-keys %p1)))
    (make-array (length keys)
                :adjustable t
                :fill-pointer t
                :initial-contents keys)))

(native "len" (hash) (hash-table-count %p1))
(native "remove" (hash any) (%bool (remhash %p2 %p1)))

;; User-defined-types
(native ".=" (any any any)
  (let ((target %p1))
     (when (not (typep target 'ngs-object))
       (error 'parameters-mismatch))
     (let ((v %p3))
       (setf (gethash %p2 (ngs-object-attributes target)) v)
       v)))

(native "." (any any)
  (let ((target %p1))
     (when (not (typep target 'ngs-object))
       (error 'parameters-mismatch))
     (gethash %p2 (ngs-object-attributes target))))

;; File
(defun file-string (path)
  "http://rosettacode.org/wiki/Read_entire_file#Common_Lisp"
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))
(native "File" (string) (parse-namestring %p1))
(native "fetch" (file) (file-string %p1))
(native "store" (file string)
  (with-open-file (stream %p1 :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (write-sequence %p2 stream)))

;; Not sure about correctness
(native-getattr file
  ("name" (format nil "~A" %p1)))

;; Regexp
;; TODO: performance: cache the mapping of string->regex
(native "Regexp" (string string)
  ;; (format t "Regexp(~A,~A)" %p1 %p2)
  (let ((ret (apply #'cl-ppcre:create-scanner
                    %p1
                    (apply #'append (loop
                                       for modifier across %p2
                                       collecting (list (cdr (assoc modifier *regexp-flags*)) t))))))
    (setf (gethash ret *ngs-objects-types*) :regexp)
    ret))

;; TODO: consider returning an array
(native "~" (string regexp) (multiple-value-bind (whole groups) (cl-ppcre:scan-to-strings %p2 %p1)
                              (if whole
                                  (concatenate 'list (list whole) groups)
                                  :null)))

;; TODO: consider returning an array
(native "~~" (string regexp) (cl-ppcre:all-matches-as-strings %p2 %p1))
;; (native "replace" (string regexp string) (cl-ppcre:regex-replace ...

;; TODO: v1, v2, v3 = "str" ~ "regexp(with).*(groups)"
;; TODO: consider returning an array
(native "split" (string regexp) (cl-ppcre:split %p2 %p1))

(native "echo" (any)
  (let ((v (%call "String" (make-arguments :positional (list %p1)))))
    (format t "~A~%" v)
    v))

;; TODO: Improve throw/catch because it's now simple
(native "throws" (any any) (let ((b (%call "Bool" (make-arguments :positional (list %p1)))))
                             (ecase b
                               (:true (error 'ngs-user-exception :datum %p2))
                               (:false %p1))))

;; TODO: consider removing handler-case for runtime-error in ngs-compile when called from here
(native "compile" (string string) (ngs-compile %p1 %p2))

;; TODO: type assertion
(native "load" (any) (lambda (load-generated-lambda-params) (declare (ignore load-generated-lambda-params)) (eval %p1)))

(native "meta" (any) (multiple-value-bind (result found) (gethash %p1 *ngs-meta*)
                       (if found
                           result
                           (setf (gethash %p1 *ngs-meta*) (make-hash-table :test #'equal)))))

;; Hack attack!
(let ((orig (fdefinition 'yason::parse-constant)))
  (setf (fdefinition 'yason::parse-constant) (lambda (input)
                                               (let ((x (funcall orig input)))
                                                 (cond
                                                   ((eq x t) :true)
                                                   ((eq x nil) :false)
                                                   (t x))))))

;; Hack attack!
(setf (fdefinition 'yason::create-container)
      (lambda ()
        (ecase yason::*parse-object-as*
          ((:plist :alist)
           nil)
          (:hash-table
           (make-hash-table :test #'equal)))))

(native "from_json" (string) (yason:parse %p1 :json-nulls-as-keyword t :json-arrays-as-vectors t))

;; WARNING: Lisp implementation specific code

(ngs-define-function "Process" *ngs-globals* t
                     (lambda (parameters)
                       (let* ((positionals %positionals)
                              (p
                              (sb-ext:run-program
                               (car positionals)
                               (cdr positionals)
                               :output :stream
                               :wait nil
                               :search t)))
                         (setf (gethash p *ngs-objects-attributes*) `(("argv" ,positionals)))
                         p)))

(native "wait" (process) (sb-ext:process-wait %p1))

(native-getattr process
  ("argv" (second (assoc "argv" (gethash %p1 *ngs-objects-attributes*) :test #'equal)))
  ("code" (%nil->nul (sb-ext:process-exit-code %p1)))
  ("status" (string-downcase (symbol-name (sb-ext:process-status %p1))))
  ("stdout" (sb-ext:process-output %p1)))

;; Stream

(native "Stream" () (make-string-output-stream))
(native "Stream" (string) (make-string-input-stream %p1))

(defun read-whole-stream (stream buf-size)
  (with-output-to-string (ret)
    (let ((buf (make-array buf-size :element-type 'character
                           :adjustable t
                           :fill-pointer buf-size)))
      (loop
         (setf (fill-pointer buf) (read-sequence buf stream))
         (when (zerop (fill-pointer buf)) (return))
         (write-sequence buf ret)))))

(native "read" (stream number) (read-whole-stream %p1 %p2))
(native "read" (stream) (read-whole-stream %p1 *read-buffer-size*))
(native "read_char" (stream) (coerce (list (read-char %p1)) 'string))
(native "read_byte" (stream) (read-byte %p1))
(native "read_line" (stream) (read-line %p1))

;; system misc
(native "exit" (number) (sb-ext:exit :code %p1))

;; variables

(set-var "stdin"  *ngs-globals* *standard-input*)
(set-var "stdout" *ngs-globals* *standard-output*)
(set-var "stderr" *ngs-globals* *error-output*)

(defun get-argv () sb-ext:*posix-argv*)
(defun get-env (name) (sb-posix:getenv name))
(defun get-pid () (sb-posix:getpid))

(defun get-all-env ()
  (let ((h (make-hash-table :test 'equal)))
    (loop
       for s in (sb-ext:posix-environ)
       do (let ((p (position #\= s)))
            (setf (gethash (subseq s 0 p) h) (subseq s (1+ p)))))
    h))

;; XXX: temp
(defun get-ngs-folder ()
  (or
   (get-env "NGS_FOLDER")
   (let ((dir (directory-namestring (pathname (first (get-argv))))))
     (cond
       ((equal dir "") ".")
       (t (subseq dir 0 (1- (length dir))))))))

(defun update-runtime-info ()
  ;; Remember to update if fork() is supported
  (set-var "PID" *ngs-globals* (get-pid))
  (set-var "ARGV" *ngs-globals* (%array (get-argv)))
  (set-var "ENV" *ngs-globals* (get-all-env))
  (set-var "NGS_FOLDER" *ngs-globals* (get-ngs-folder)))


;; Runtime - end ------------------------------
