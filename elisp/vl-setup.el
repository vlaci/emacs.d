;;; vl-setup.el --- setup.el extensions -*- lexical-binding: t -*-
;; Package-Requires: ((setup "1.3.2"))
(require 'setup)

(setup-define :package
  (lambda (package))
  :documentation "Fake installation of PACKAGE."
  :repeatable t
  :shorthand #'cadr)

(setup-define :nixpkgs
  (lambda (&rest nixpkgs))
  :documentation "Fake installation of packages from NIXPKGS for executables.")

(setup-define :after-gui
  (lambda (&rest body)
    `(let ((hook (if (daemonp)
                     'server-after-make-frame-hook
                   'after-init-hook)))
       (add-hook hook (lambda () ,@body))))
  :documentation "Run BODY once after the first GUI frame is created."
  :debug '(setup)
  :indendt 0)

(setup-define :load-after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t
  :signature '(FUNC ...))

(setup-define :smartparens
  (lambda (&rest body)
    `(:with-feature smartparens
       (:when-loaded
         (sp-with-modes ',(setup-get 'feature)
           ,@body))))
  :documentation "Define local pairs for smartparens.")

(with-eval-after-load 'lisp-mode
  (let ((pattern (rx-to-string
                  `(seq "(setup"
                        (+ space)
                        (|
                         (group-n 1
                           (regexp ,lisp-mode-symbol-regexp))
                         (seq "("
                              (regexp ,lisp-mode-symbol-regexp)
                              (+ space)
                              (group-n 1
                                (regexp ,lisp-mode-symbol-regexp)))))
                  'nogroup)))
    (add-to-list 'lisp-imenu-generic-expression `("Setup" ,pattern 1))))

(provide 'vl-setup)
;;; vl-setup.el ends here
