;; apt-get install cl-xlunit
;; apt-get install cl-split-sequence

(require :asdf)
(require :xlunit)
(require :split-sequence #p"/usr/share/common-lisp/source/cl-split-sequence/split-sequence.lisp")

(require :ngs '("ngs.lisp"))

(defpackage :ngs-xlunit
  (:use :cl :xlunit)
  (:export :math-test-suite))

(in-package :ngs-xlunit)

(defclass test-1 (test-case) ())

(defmacro eql-test (name expr result)
  `(def-test-method ,name ((test test-1) :run nil)
     (assert-eql ,result (ngs:value-data (eval (ngs:ngs-compile ,expr "<test>"))))))

;; For development
(setf (tests (get-suite test-1)) nil)

(defstruct ngs-test name code expected-result)

;; Based on http://rosettacode.org/wiki/Tokenize_a_string#Common_Lisp
(defun split-tests (string)
  (loop for start = 0 then (+ 2 finish)
     for finish = (search (format nil "~%~%") string :start2 start)
     collecting (subseq string start finish)
     until (null finish)))

(defun parse-test (test-string)
  (let*
      ((lines (split-sequence:split-sequence #\Newline test-string))
       (name (first lines))
       (code (format nil "~{~a~^~%~}" (butlast (rest lines))))
       (expected-result (with-input-from-string (s (first (last lines)))
                          ;; (format t "STR: <<~S>>~%" (first (last lines)))
                          (read s))))
    (make-ngs-test :name name :code code :expected-result expected-result)))

(defmacro setup-tests ()
  (let ((test-cases
         (mapcar
          #'parse-test
          (split-tests (string-right-trim '(#\Newline) (ngs::file-string "test-cases.txt"))))))
    `(progn
       ,@(loop
            for tc in test-cases
            collecting `(eql-test ,(ngs-test-name tc) ,(ngs-test-code tc) ,(ngs-test-expected-result tc))))))

(setup-tests)

(textui-test-run (get-suite test-1))
