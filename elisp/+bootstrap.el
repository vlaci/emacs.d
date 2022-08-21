;;; bootstrap.el --- summary -*- lexical-binding: t -*-

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
(require 'xdg)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      idle-update-delay 1.0
      inhibit-compacting-font-caches t
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      initial-major-mode 'fundamental-mode)

(setq use-dialog-box nil)

(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)
(setq auto-mode-case-fold nil
      read-process-output-max (* 64 1024 1024))

(require '+config)

(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache" user-emacs-directory))

(setq user-emacs-directory (expand-file-name "emacs" (xdg-config-home))
      package-user-dir (expand-file-name "emacs/package" (xdg-data-home))
      native-comp-deferred-compilation nil ;;t
      package-enable-at-startup (not +nix-build?))

(require 'nix-integration)

(unless +nix-build?
  (require 'package)
  (defvar package-archives)
  (startup-redirect-eln-cache "eln-cache")
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(provide '+bootstrap)

;;; +bootstrap.el ends here
