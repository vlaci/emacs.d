;;; packages.el --- summary -*- lexical-binding: t -*-

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

(defmacro +install! (pkg &optional no-require)
  "Install PKG when used outside of Nix build."
  (list
   #'progn
   (unless +nix-build?
     `(when (not (package-installed-p ',pkg))
        (package-install ',pkg)))
  (when (and (not no-require) (bound-and-true-p byte-compile-current-file) (not (featurep pkg)))
    `(require ',pkg))))

(defmacro +define-key! (pkg keymap key def &optional remove)
  (list
   #'progn
   `(unless (fboundp ',def)
     (autoload #',def ,(symbol-name pkg) nil t))
   `(eval-when-compile
     (declare-function ,def ,(symbol-name pkg)))
   `(if (boundp ',keymap)
       (progn
         (defvar ,keymap)
         (define-key ,keymap ,key #',def ,remove))
     (eval-after-load
         ',pkg
       '(define-key ,keymap ,key #',def ,remove)))))

(defmacro +after! (feature &rest body)
  (declare (indent defun)(debug t))
  (when (bound-and-true-p byte-compile-current-file)
                (require feature nil 'noerror))
  `(eval-after-load ',feature ',(macroexp-progn body)))

(when +nix-build?
  ;; If built with nix, we have precomputed autoloads that we should load
  (load "autoloads")
  ;; Otherwise load package definitions
  (message "loading packages")
  (package-activate-all))

(provide '+packages)

;;; +packages.el ends here
