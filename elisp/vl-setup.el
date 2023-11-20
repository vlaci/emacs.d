;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'setup)
  (require 'cl-lib)
  (defmacro define-setup-macro (name signature &rest body)
    "Shorthand for `setup-define'.
NAME is the name of the local macro.  SIGNATURE is used as the
argument list for FN.  If BODY starts with a string, use this as
the value for :documentation.  Any following keywords are passed
as OPTS to `setup-define'."
    (declare (debug defun))
    (let (opts)
      (when (stringp (car body))
        (setq opts (nconc (list :documentation (pop body))
                          opts)))
      (while (keywordp (car body))
        (let* ((prop (pop body))
               (val `',(pop body)))
          (setq opts (nconc (list prop val) opts))))
      `(setup-define ,name
                     (cl-function (lambda ,signature ,@body))
                     ,@opts))))

(eval-when-compile
  (define-setup-macro :package (package)
                      "Fake installation of PACKAGE."
                      :repeatable t
                      :shorthand cadr))

(eval-when-compile
  (define-setup-macro
   :set (&rest args)
   "Set the default values of variables."
   `(setq-default ,@args)))

(eval-when-compile
(setup-define :after-gui
  (lambda (&rest body)
    `(let ((hook (if (daemonp)
                     'server-after-make-frame-hook
                   'after-init-hook)))
       (add-hook hook (lambda () ,@body))))
  :documentation "Run BODY once after the first GUI frame is created."
  :debug '(setup)
  :indendt 0))

(provide 'vl-setup)
