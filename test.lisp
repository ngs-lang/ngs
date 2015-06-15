;; apt-get install cl-xlunit

(require :asdf)
(require :xlunit)

(require :ngs '("ngs.lisp"))

(defpackage :ngs-xlunit
  (:use :cl :xlunit)
  (:export :math-test-suite))

(in-package :ngs-xlunit)

(defclass test-1 (test-case) ())

(defmacro eql-test (name expr result)
  `(def-test-method ,name ((test test-1) :run nil)
	 (assert-eql ,result (ngs:value-data (eval (ngs:ngs-compile ,expr "<test>"))))))

;; (def-test-method addition ((test test-1) :run nil)
;;   (assert-eql 3 (ngs:value-data (eval (ngs:ngs-compile "1+2" "<test>")))))

;; For development
(setf (tests (get-suite test-1)) nil)

(eql-test addition "1+2" 3)
(eql-test local-var "x=1; def f() {2}; f(); x" 1)


(textui-test-run (get-suite test-1))
