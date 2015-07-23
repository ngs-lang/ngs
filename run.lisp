(require :sb-posix)
(require :ngs '("ngs.lisp"))

(in-package :ngs)

(defun main ()
  (when (sb-posix:getenv "NGS_TRACE")
    (trace "NGS"))
  (when (sb-posix:getenv "NGS_PROFILE")
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
                        ngs::ngs-call-function))
  (let* ((argv (get-argv))
         (file-name (second argv))
         (source-code (file-string file-name))
         (code (ngs-compile source-code file-name)))
    ;; (format t "~S~%" code)
    (eval code))
  (when (sb-posix:getenv "NGS_PROFILE")
    (sb-profile:report)))

(if (sb-posix:getenv "NGS_COMPILE")
    (sb-ext:save-lisp-and-die "ngs" :toplevel #'main :executable t)
    (main))

