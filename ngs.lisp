;; apt-get install cl-esrap

(require :asdf)
(require :esrap)

(defpackage :ngs
  (:use :cl :esrap)
  (:export
   #:ngs-call-function))
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


(defun make-binop-node (ls)
  (let ((result (first ls)))
	(loop
	   for (op expr) in (second ls)
	   do (setq result (make-instance 'binary-operation-node :data (second op) :children (list result expr))))
	result))

(defrule optional-sign (or "+" "-" ""))

(defrule digits (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

(defrule inline-space (+ (or #\Space #\Tab)) (:constant nil))

(defrule space (+ (or #\Space #\Tab #\Newline)) (:constant nil))

(defrule optional-space (* space) (:constant nil))

(defrule integer (and optional-sign digits) (:lambda (list) (parse-integer (text list) :radix 10)))

(defrule float (and optional-sign digits "." digits) (:lambda (list) (with-input-from-string (s (text list)) (read s))))

(defrule number (or float integer) (:lambda (n) (make-instance 'number-node :data n)))

(defrule letters (character-ranges (#\a #\z) (#\A #\Z)) (:lambda (list) (text list)))

(defrule identifier-immediate (and (+ letters) (* digits))
  (:lambda (list)
	(make-instance 'string-node :data (text list))))

(defrule identifier (or identifier-immediate))

(defrule expression (or function-definition binary-expression-1))

(defrule varname (and (+ letters) (* digits)) (:lambda (list) (make-instance 'varname-node :data (text list))))

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

(defrule expressions (and expression (* (+ (and optional-space ";" optional-space expression optional-space))) (* ";"))
  (:lambda (list)
	(make-instance 'expressions-node :children (append (list (first list)) (mapcar #'fourth (caadr list))))))

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
							 identifier
							 optional-space
							 (? (and optional-space ":" optional-space varname))
							 (? (and optional-space "=" optional-space expression)))
  (:lambda (list)
	(make-instance 'function-parameter-node :children (list
													   (first list)
													   (fourth (third list))
													   (fourth (fourth list))))))

(defrule function-parameters (? (and function-parameter (* (and optional-space "," optional-space function-parameter optional-space))))
  (:lambda (list)
	;; (format t "~%WTF1: ~S~%" (make-instance 'function-parameters-node :children (append (list (first list)) (mapcar #'fourth (cadr list)))))
	(make-instance 'function-parameters-node :children (append (list (first list)) (mapcar #'fourth (cadr list))))))


(defrule-spaced-seq function-parameters-with-parens ("(" function-parameters ")") (:lambda (list) (third list)))

(defrule-spaced-seq function-definition ("def" space identifier function-parameters-with-parens "{" (? expressions) "}")
  (:lambda (list)
	(make-instance 'function-definition-node :children (list
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
	(format t "ARG: ~S~%" list)
	(make-instance 'function-argument-node :children (list list))))


;; TODO: named arguments
(defrule function-arguments (? (and function-argument (* (and optional-space "," optional-space function-argument optional-space))))
  (:lambda (list)
	(format t "ARGS: ~S~%" list)
	(make-instance 'function-arguments-node :children (when list (append (list (first list)) (mapcar #'fourth (cadr list)))))))

(defrule-spaced-seq function-call (expression "(" function-arguments ")")
  (:lambda (list)
	(make-instance 'function-call-node :children (list (first list) (fifth list)))))

(defrule non-binary-operation (or
							   assignment
							   function-call
							   number
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

(defun one-level-deeper-lexical-vars (ls)
  (make-instance 'lexical-scopes :hashes (cons (make-hash-table :test #'equal :size 20) (lexical-scopes-hashes ls))))

(defun getvar (name vars)
  (let ((key (value-data name)))
	(format t "GETVAR ~S ~S~%" key vars)
	(loop for hash in (lexical-scopes-hashes vars)
	   do (multiple-value-bind (result found) (gethash key hash)
			(when found (return-from getvar (values result hash)))))
	(error 'variable-not-found :varname name)))

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
  (setf (gethash name (first (last (lexical-scopes-hashes *ngs-globals*)))) value))

(format t "LS: ~S~%" *ngs-globals*)
;; Variables - end ------------------------------


;; Types definitions - start ------------------------------

(defstruct ngs-type name parents constructors)

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
(def-ngs-type "Null")

;; Types definitions - end ------------------------------

;; Compiler - start ------------------------------

(define-symbol-macro %1 (generate-code (first (node-children n))))
(define-symbol-macro %2 (generate-code (second (node-children n))))
(define-symbol-macro %children (children-code n))
(define-symbol-macro %data (node-data n))

(defun children-code (node &key (start 0)) (mapcar #'generate-code (subseq (node-children node) start)))

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
																collecting `(setvar (first (nth ,i expected-parameters)) vars
																				 (if
																				  (> (length (arguments-positional parameters)) ,i)
																				  (nth ,i (arguments-positional parameters))
																				  ,(if (third pc)
																					   `(third (nth ,i expected-parameters))
																					   `(error 'parameters-mismatch)))))))

(defmethod generate-code ((n function-call-node))       `(ngs-call-function ,%1 ,%2))
(defmethod generate-code ((n function-arguments-node))  `(make-arguments
														  :positional
														  (list ,@(loop
																	 for a in (node-children n) ; a is function-argument-node
																	 collecting (generate-code (first (node-children a)))))))

(defun ngs-compile (code)
  (let ((c (generate-code (parse 'expressions code))))
	`(let ((vars *ngs-globals*))
	   ,c)))

;; Compiler - end ------------------------------

;; Runtime - start ------------------------------


(define-condition variable-not-found () ((varname :initarg :varname)))

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

(defun ngs-define-function (function-name vars expected-parameters lambda)
  (let ((v (getvar-or-default function-name vars nil)))
	(setvar function-name vars (cons lambda v)))) ; XXX

(defun ngs-call-function (methods arguments)
  (loop for f in methods
	 ;; do (format t "+ Trying implementation ~A~%" f)
	 return (funcall f arguments)))


(define-symbol-macro %positionals (mapcar #'value-data (arguments-positional parameters)))

(ngs-define-function (mk-string "+") *ngs-globals* nil (lambda (parameters) (mk-number (apply #'+ %positionals))))

;; Runtime - end ------------------------------

;; (princ (eval (generate-code (parse 'expression "1+2+3"))))
;; (let ((c (generate-code (parse 'expressions "a=1; def f { a }; f();"))))

(defun main ()
  ;; (let ((c (generate-code (parse 'expressions "def f(x:Number, y:Number=5) {x+y}; f(10,20);"))))
  (let ((c (ngs-compile "def f(x:Number, y:Number=5) {x+y}; f(10,20);")))
  ;; (let ((c (generate-code (parse 'function-parameters-with-parens "(x:Number=3, y:Number=5)"))))
	(format t "CODE: ~S~%" c)
	(format t "RESULT: ~S~%" (eval c))))
	;; ))

;; defg fib(x) { fib(x-1) + fib(x-2) }
;; defg fib(x<=2) { 1 }
;; defg fib(x<=0) { 0 }

;; (format t "COMPILING~%")
;; (sb-ext:save-lisp-and-die "ngs-bin" :toplevel #'main :executable t)
(main)
