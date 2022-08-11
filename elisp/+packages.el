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

(defmacro +install! (pkg)
  "Install PKG when used outside of Nix build."
  `(when (and (not +nix-build?) (not (package-installed-p ',pkg)))
     (package-install ',pkg)))

(when +nix-build?
  ;; If built with nix, we have precomputed autoloads that we should load
  (load "autoloads")
  ;; Otherwise load package definitions
  (message "loading packages")
  (package-activate-all))

(provide '+packages)

;;; +packages.el ends here
