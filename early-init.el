;;; early-init.el --- summary -*- lexical-binding: t -*-
;;; Commentary:

;; commentary

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(add-to-list 'load-path (expand-file-name "elisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "modules" (file-name-directory load-file-name)))

(load "+bootstrap")

(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)
    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (declare-function +reset-file-handler-alist ())
    (defun +reset-file-handler-alist ()
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'+reset-file-handler-alist 101)))
