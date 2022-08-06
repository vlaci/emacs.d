;;; +lib.el --- Common utilities for my Emacs configuration -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require '+config)
(require '+set-defaults)

(when +nix-build?
  ;; If built with nix, we have precomputed autoloads that we should load
  (load "autoloads"))

(defmacro +set-defaults! (&rest args)
  "Override default of `defcustom' variable VAR to VALUE.

\(fn [VAR VALUE]...)"
  (let ((pairs (cl-loop for (k v) on args by #'cddr
                    collect `'(,k ,v))))
    `(set-defaults ,@pairs)))

(defmacro +install! (pkg &optional no-require)
  "Install PKG when used outside of Nix build.
Require PKG during byte-compoilation unless NO-REQUIRE is set."
  (list
   #'progn
   (unless +nix-build?
     `(when (not (package-installed-p ',pkg))
        (package-install ',pkg)))
   (when (and (not no-require) (or (not +nix-build?) (bound-and-true-p byte-compile-current-file)) (not (featurep pkg)))
     `(require ',pkg nil 'noerror))))

(defmacro +define-key! (pkg keymap key def &optional remove)
  (list
   #'progn
   `(unless (fboundp ,def)
      (autoload ,def ,(symbol-name pkg) nil t))
   `(eval-when-compile
      (declare-function ,def ,(symbol-name pkg)))
   `(if (boundp ',keymap)
        (progn
          (defvar ,keymap)
          (define-key ,keymap ,key ,def ,remove))
      (eval-after-load
          ',pkg
        '(define-key ,keymap ,key ,def ,remove)))))

(require 'cl-macs)

(defmacro +define-keys! (pkg &rest definitions)
  (declare (indent defun)(debug t))
  (let (forms)
    (dolist (bind-specs definitions)
      (cl-destructuring-bind (keymap defs) bind-specs
        (dolist (bind-spec defs)
          (cl-destructuring-bind (key def) bind-spec
            (push `(+define-key! ,pkg ,keymap ,key ,def) forms)))))
    `(progn ,@(reverse forms))))

(defmacro +after! (feature &rest body)
  (declare (indent defun)(debug t))
  (when (bound-and-true-p byte-compile-current-file)
                (require feature nil 'noerror))
  `(eval-after-load ',feature ',(macroexp-progn body)))

(defsubst +normalize-direction (direction)
  (cond
   ((memq direction '(left leftmost)) '(left . right))
   ((memq direction '(right rightmost)) '(right . left))
   ((memq direction '(above top up)) '(above . below))
   ((memq direction '(below bottom down)) '(below . above))
   (t '(below above))))

(defun +display-buffer-in-direction (buffer alist)
  "Try to display BUFFER at edge specified in ALIST."
  (let* ((direction (alist-get 'direction alist))
         (reference-window (+find-window direction))
         (selected-window (selected-window))
         (should-split (or (eq reference-window selected-window)
                           (>= (window-width reference-window) split-width-threshold)))
         (nd (+normalize-direction direction))
         (window-pos (car nd))
         (split-dir (if (eq window-pos direction) (cdr nd) window-pos))
         (window (progn (delete-other-windows-vertically reference-window)
                        (if should-split
                            (split-window reference-window nil split-dir)
                          reference-window))))
    (window--display-buffer buffer window (if should-split 'window 'reuse) alist)
    (set-window-parameter window 'quit-restore (list 'window 'window selected-window buffer))
    (when should-split
      (set-window-prev-buffers window nil))))

(defun +find-window (direction &optional window)
  "Find the window at the given DIRECTION starting from WINDOW or current window."
  (let* ((edge '(leftmost rightmost top bottom))
         (window (or window (selected-window)))
         (nd (car (+normalize-direction direction)))
         (new-window (window-in-direction nd window)))
    (cond ((and new-window (memq direction edge))
           (+find-window direction new-window))
          (new-window new-window)
          (t window))))

(defun +advised-executable-find (executable-find-fn &rest args)
  (let ((exec-path (append +extra-exec-path exec-path)))
    (apply executable-find-fn args)))

(advice-add 'executable-find :around #'+advised-executable-find)

(provide '+lib)

;;; +lib.el ends here
