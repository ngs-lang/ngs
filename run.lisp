(require :sb-posix)
(require :ngs '("ngs.lisp"))

(in-package :ngs)

(defun main ()
  (when (get-env "NGS_TRACE")
    (ngs::trace-rule 'expressions :recursive t))
  (when (get-env "NGS_PROFILE")
    (sb-profile:profile ngs-compile
                        esrap::parse
                        ngs::generate-code
                        file-string
                        eval
                        ngs-define-function
                        guard-type
                        one-level-deeper-lexical-vars
                        set-local-var
                        get-var
                        make-arguments
                        concatenate
                        make-array
                        ;; "NGS"
                        ngs::string-sq-not-ending-char
                        ngs::string-dq-not-ending-char
                        ngs::string-regexp-not-ending-char
                        ;; "ESRAP"
                        ngs::ngs-call-function))
  (let* ((argv (get-argv))
         (file-name (second argv))
         (source-code (file-string (format nil "~A/~A" (get-ngs-folder) "bootstrap.ngs")))
         (code (ngs-compile source-code file-name)))
    (when (get-env "NGS_CODE")
      (format t "~S~%" code))
    (ngs:update-runtime-info)
    (eval code))
  (when (get-env "NGS_PROFILE")
    (sb-profile:report)))

(if (get-env "NGS_COMPILE")
    (progn
      (setq *running-as-binary* t)
      (sb-ext:save-lisp-and-die "ngs" :toplevel #'main :executable t))
    (main))

