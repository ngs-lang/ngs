(require :ngs '("ngs.lisp"))

(in-package :ngs)

(defun main ()
  (let* ((argv (get-argv))
         (file-name (second argv))
         (source-code (file-string file-name))
         (code (ngs-compile source-code file-name)))
    (eval code)))

(main)
