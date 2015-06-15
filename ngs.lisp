;; apt-get install cl-esrap

(require :asdf)
(require :esrap)

(defpackage :ngs
  (:use :cl :esrap)
  (:export
   :ngs-call-function
   :ngs-compile
   :value :value-data))

(in-package :ngs)

(defstruct parameters positional named)
(defstruct arguments  positional named)

;; Parser - start ------------------------------


(defclass node ()
  ((src :initarg :src :initform nil :accessor node-src)
   (children :initarg :children :initform (list) :accessor node-children)
   (data :initarg :data :initform nil :accessor node-data)))

(defparameter *required-space-binary-operators*
  '(("or")
	("and")
	("in" "not in")))

(defparameter *optional-space-binary-operations*
  '(("*" "/")
	("+" "-")))

(defparameter *binary-operators*
  (append
   (mapcar #'(lambda(x) `(and space (or ,@x) space)) *required-space-binary-operators*)
   (mapcar #'(lambda(x) `(and optional-space (or ,@x) optional-space)) *optional-space-binary-operations*)))

(defmethod print-object ((n node) stream)
  (format stream "#<~A :SRC ~A :DATA ~A :CHILDREN ~A>"
		  (class-name (class-of n))
		  (node-src n)
		  (node-data n)
		  (node-children n)))

(defclass number-node (node) ())
(defclass string-node (node) ())
(defclass binary-operation-node (node) ())
(defclass identifier-node (node) ())
(defclass varname-node (node) ())
(defclass assignment-node (node) ())
(defclass expressions-node (node) ())

(defclass function-definition-node (node) ())
(defclass function-parameter-node (node) ())
(defclass function-parameters-node (node) ())
(defclass function-argument-node (node) ())
(defclass function-arguments-node (node) ())
(defclass function-call-node (node) ())

(defclass incompilable-node (node) ())
(defclass comment-node (incompilable-node) ())
(defclass end-node (incompilable-node) ())


(defun make-binop-node (ls)
  (let ((result (first ls)))
	(loop
	   for (op expr) in (second ls)
	   do (setq result (make-instance 'binary-operation-node :data (second op) :children (list result expr))))
	result))

(define-symbol-macro %std-src (list 'list (make-human-position start) (make-human-position end)))

(defrule comment (and #\# (* (and (! #\Newline) character)))
  (:lambda (list &bounds start end)
	(declare (ignore list))
	(make-instance 'comment-node :src %std-src)))

(defrule end (and "END" #\Newline (* (string 1)))
  (:lambda (list &bounds start end)
	(declare (ignore list))
	(make-instance 'end-node :src %std-src)))

(defrule optional-sign (or "+" "-" ""))

(defrule digits (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

(defrule inline-space (+ (or #\Space #\Tab)) (:constant nil))

(defrule space (+ (or #\Space #\Tab #\Newline)) (:constant nil))

(defrule newline-space (+ (and (? inline-space) (+ #\Newline) (? inline-space))))

(defrule optional-space (* space) (:constant nil))

(defrule integer (and optional-sign digits) (:lambda (list) (parse-integer (text list) :radix 10)))

(defrule float (and optional-sign digits "." digits) (:lambda (list) (with-input-from-string (s (text list)) (read s))))

(defrule number (or float integer) (:lambda (n &bounds start end) (make-instance 'number-node :data n :src %std-src)))

(defrule string (and #\" #\") (:lambda (&bounds start end) (make-instance 'string-node :data "" :src %std-src)))

(defrule letters (character-ranges (#\a #\z) (#\A #\Z)) (:lambda (list) (text list)))

(defrule identifier-first (+ (or letters "_")))

(defrule identifier-rest (* (or identifier-first digits)))

(defrule identifier-whole-text (and identifier-first identifier-rest)
  (:lambda (list) (text list)))

(defrule identifier-immediate identifier-whole-text
  (:lambda (list &bounds start end)
	(make-instance 'string-node :data list :src %std-src)))

(defrule identifier (or identifier-immediate))

(defrule expression (or comment end function-definition binary-expression-1))

(defrule varname identifier-whole-text
  (:lambda (list &bounds start end)
	(make-instance 'varname-node :data list :src %std-src)))

(defun %bin-expr (n)
  (intern (concatenate 'string "BINARY-EXPRESSION-" (write-to-string n))))

(defmacro define-binary-operations-rules ()
  `(progn
	 ,@(loop
		  for binary-operator in *binary-operators*
		  for i from 1
		  collecting
		  ;; http://en.wikipedia.org/wiki/Operator-precedence_parser
			`(defrule ,(%bin-expr i) (and ,(%bin-expr (1+ i)) (* (and ,binary-operator ,(%bin-expr (1+ i)))))
			   (:lambda (list)
				 (make-binop-node list))))
	 (defrule ,(%bin-expr (1+ (length *binary-operators*))) non-binary-operation
	   (:lambda (list) list))))

(defrule expressions (and expression (* (+ (and expressions-delimiter expression))) optional-space (* ";"))
  (:lambda (list)
	(make-instance 'expressions-node :children (append (list (first list)) (mapcar #'second (caadr list))))))

(defrule expressions-delimiter (or newline-space (and (? inline-space) ";" (? space))))

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
						  (cond ((eq "*"  (first list)) 'positional-rest)
								((eq "**" (first list)) 'named-rest)
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

(define-binary-operations-rules)

(defrule assignment (and identifier optional-space "=" optional-space expression)
  (:lambda (list)
	(make-instance 'assignment-node :children (list (first list) (fifth list)))))

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

(defrule non-binary-operation (or
							   assignment
							   function-call
							   number
							   string
							   varname))

;; Parser - end ------------------------------

;; Variables - start ------------------------------

(defstruct value data type meta id)

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

(defun one-level-deeper-lexical-vars (ls)
  (make-instance 'lexical-scopes :hashes (cons (make-hash-table :test #'equal :size 20) (lexical-scopes-hashes ls))))

(defun getvar (name vars)
  (let ((key (value-data name)))
	;; (format t "GETVAR ~S ~S~%" key vars)
	(loop for hash in (lexical-scopes-hashes vars)
	   do (multiple-value-bind (result found) (gethash key hash)
			(when found (return-from getvar (values result hash)))))
	(error 'variable-not-found :varname name :stack-trace *source-position*)))

(defun getvar-or-default (name vars default)
  (handler-case (getvar name vars)
	(variable-not-found () default)))

(defun setvar (name vars value)
  (let ((dst-hash
		 (handler-case (multiple-value-bind (unused-result hash) (getvar name vars) hash)
		   (variable-not-found () (first (lexical-scopes-hashes vars))))))
	(setf (gethash (value-data name) dst-hash) value)))

(defun %set-global-variable (name value)
  ;; (format t "X ~S ~%" (last (lexical-scopes-hashes *ngs-globals*)))
  (setf (gethash name (first (lexical-scopes-hashes *ngs-globals*))) value))

;; Variables - end ------------------------------


;; Types definitions - start ------------------------------

(defstruct ngs-type name parents constructors)

(defmethod print-object ((typ ngs-type) stream)
  (format stream "#<ngs-type ~A>" (ngs-type-name typ)))

(defun %ngs-type-symbol (type-name)
  (intern (concatenate 'string "NGS-TYPE-" (string-upcase type-name))))

(defmacro def-ngs-type (name)
  (let*
	  ((symb (%ngs-type-symbol name)))
	`(progn
	   (defvar ,symb
		 (make-ngs-type :name ,name))
	   (defun ,(intern (concatenate 'string "MK-" (string-upcase name))) (data)
		   (make-value :type ,symb :data data))
	   (%set-global-variable ,name ,symb))))

(def-ngs-type "Any")
(def-ngs-type "Type")
(def-ngs-type "Number")
(def-ngs-type "String")
(def-ngs-type "List")
(def-ngs-type "Array")
(def-ngs-type "Null")

;; Types definitions - end ------------------------------

;; Compiler - start ------------------------------

(defvar *source-file-name* "TOP-LEVEL")
(defvar *source-file-positions* #(0))

(defun make-human-position (position)
  (let ((line (loop
				 for p across *source-file-positions*
				 for line from 0
				 if (or (eq line (1- (length *source-file-positions*)))
						(> (elt *source-file-positions* (1+ line)) position)) return line)))
	(format nil "~A:~A:~A" *source-file-name* (1+ line) (1+ (- position (elt *source-file-positions* line))))))

(define-symbol-macro %1 (generate-code (first (node-children n))))
(define-symbol-macro %2 (generate-code (second (node-children n))))
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

(defgeneric generate-code (node))

;; For simplicity of generate-expected-parameters, which has nullable fields in each parameter

(defmethod generate-code ((n null))                      nil)

(defmethod generate-code ((n number-node))               (mk-number %data))
(defmethod generate-code ((n string-node))               (mk-string %data))
(defmethod generate-code ((n varname-node))             `(getvar ,(mk-string %data) vars))
(defmethod generate-code ((n binary-operation-node))    `(ngs-call-function
														  (getvar ,(mk-string %data) vars)
														  (make-arguments :positional (list ,@(children-code n)))))
(defmethod generate-code ((n assignment-node))          `(setvar
														  (mk-string ,(node-data (first (node-children n))))
														  vars
														  ,@(children-code n :start 1)))
(defmethod generate-code ((n expressions-node))         `(progn ,@%children))

(defmethod generate-code ((n function-definition-node)) `(let ((expected-parameters ,(generate-expected-parameters (second (node-children n)))))
														   (ngs-define-function
															,%1
															vars
															expected-parameters
															(lambda (parameters)
															  (let ((vars (one-level-deeper-lexical-vars vars)))
																,@(children-code n :start 1))))))

;; 1. match the parameters and signal if there is a mismatch
;; 2. set local variables
;; 3. do it smarter and more efficient
(defmethod generate-code ((n function-parameters-node)) `(progn
														   ,@(loop
																for p in (node-children n)
																for pc = (node-children p)
																for i from 0
																collecting
																  (cond
																	((eq (first (node-data p)) 'positional-rest)
																	 `(setvar
																	   (first (nth ,i expected-parameters))
																	   vars
																	   (mk-array (apply #'vector (subseq (arguments-positional parameters) ,i)))))
																	(t
																	 `(setvar (first (nth ,i expected-parameters)) vars
																			  (if
																			   (> (length (arguments-positional parameters)) ,i)
																			   (nth ,i (arguments-positional parameters))
																			   ,(if (third pc)
																					`(third (nth ,i expected-parameters))
																					`(error 'parameters-mismatch)))))))))

(defmethod generate-code ((n function-call-node))       `(ngs-call-function ,%1 ,%2))
(defmethod generate-code ((n function-arguments-node))  `(make-arguments
														  :positional
														  (list ,@(loop
																	 for a in (node-children n) ; a is function-argument-node
																	 collecting (generate-code (first (node-children a)))))))

(defmethod generate-code :around ((n node))
  `(let ((*source-position* (cons ,(or (node-src n) "<unknown>") *source-position*)))
	 ,(call-next-method)))

(defun make-source-file-positions (code)
  "Positions where lines start"
  (apply #'vector 0 (loop
					 for char across code
					 for position from 0
					 if (eq #\Newline char) collecting (1+ position))))

(defun ngs-compile (code file-name)
  (let* ((*source-file-name* file-name)
		 (*source-file-positions* (make-source-file-positions code))
		 (c (generate-code (parse 'expressions code))))
	`(let ((vars *ngs-globals*))
	   (handler-case ,c
		 (runtime-error (e) (format t "Run-time error: ~A~%Stack: ~S" e (runtime-error-stack-trace e)))))))

;; Compiler - end ------------------------------

;; Runtime - start ------------------------------

(define-condition runtime-error () ((stack-trace :initarg :stack-trace :initform nil :reader runtime-error-stack-trace)))
(define-condition variable-not-found (runtime-error) ((varname :initarg :varname :reader variable-not-found-varname)))
(define-condition method-implementatoin-not-found (runtime-error) ())

(defmethod print-object ((e variable-not-found) stream)
  (format stream "Variable '~A' not found" (value-data (variable-not-found-varname e))))

;; TODO: check parents
(defun ngs-is-subtype (sub typ)
  (or
   (eq sub typ)
   nil))

(defun ngs-value-is-of-type (val typ)
  (ngs-is-subtype (value-type val) typ))

(defun assert-type (val typ)
  (unless (ngs-value-is-of-type val typ)
	(error 'parameters-mismatch)))

(defun hash-keys (h)
  (loop for key being the hash-keys of h
	 collecting key))

;; XXX - some issues, probably global/local
(defun ngs-define-function (function-name vars expected-parameters lambda)
  (let ((v (getvar-or-default function-name vars nil)))
	(if (typep v 'ngs-type)
		(setf (ngs-type-constructors v) (cons lambda (ngs-type-constructors v)))
		(setvar function-name vars (cons lambda v)))))

;; TODO - handle parameters-mismatch
(defun ngs-call-function (methods arguments)
  ;; (format t "TYPE? ~S ~S ~%" (typep methods 'ngs-type) methods)
  (if (typep methods 'ngs-type)
	  (ngs-call-function (ngs-type-constructors methods) arguments)
	  (progn
		(loop for m in methods
		   ;; do (format t "+ Trying implementation ~A~%" f)
		   do (return-from ngs-call-function (funcall m arguments)))
		(error 'method-implementatoin-not-found))))


(define-symbol-macro %positionals (mapcar #'value-data (arguments-positional parameters)))

(ngs-define-function (mk-string "+") *ngs-globals* nil (lambda (parameters) (mk-number (apply #'+ %positionals))))

(ngs-define-function (mk-string "String")
					 *ngs-globals*
					 nil
					 (lambda (parameters)
					   (mk-string (format nil "~A" (value-data (first (arguments-positional parameters)))))))

(ngs-define-function (mk-string "echo")
					 *ngs-globals*
					 nil
					 (lambda (parameters)
					   (let ((v (ngs-call-function (getvar (mk-string "String") *ngs-globals*) parameters)))
						 (format t "~A~%" (value-data v))
						 v)))

;; Runtime - end ------------------------------

(defun get-argv ()
  "Abstraction layer for ARGV"
  sb-ext:*posix-argv*)

(defun file-string (path)
  "http://rosettacode.org/wiki/Read_entire_file#Common_Lisp"
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))
