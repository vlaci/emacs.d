;;; init.el --- summary -*- lexical-binding: t -*-

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
(require 'packages)

;;; To organize runtime and config files

(+install! no-littering)

(defvar no-littering-etc-directory)
(defvar no-littering-var-directory)

(require 'xdg)
(setq no-littering-var-directory (expand-file-name "emacs" (xdg-data-home))
      no-littering-etc-directory (expand-file-name "emacs" (xdg-config-home)))

(setq custom-file (expand-file-name "settings.el" no-littering-etc-directory))

(when (not (bound-and-true-p byte-compile-current-file))
  (startup-redirect-eln-cache (expand-file-name "eln-cache" no-littering-var-directory))
  (require 'no-littering))

(load custom-file 'noerror)


;;; To improve help
(+install! helpful)
(+install! elisp-demos)

(global-set-key [remap describe-command] #'helpful-command)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-macro] #'helpful-macro)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key [remap describe-symbol] #'helpful-symbol)
(global-set-key [remap describe-variable] #'helpful-variable)

(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;;; init.el ends here
