;;; +writing.el --- summary -*- lexical-binding: t -*-

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
(require '+lib)

;;;; Spell and language checking
(use-package flymake-collection)

(defun +flymake-text-mode-setup ()
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-proselint nil t)
  (flymake-mode))

(use-package flymake
  :ensure nil
  :hook
  (text-mode . +flymake-text-mode-setup)
  :custom-face
  (flymake-error-echo ((t (:height 0.7))))
  (flymake-warning-echo ((t (:height 0.7))))
  (flymake-note-echo ((t (:height 0.7))))
  :init
  (setq flymake-show-diagnostics-at-end-of-line t))


(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct)
  :init (setq jinx-languages "en_US hu_HU"))

;;;; Editing
(use-package olivetti
  :hook ((markdown-mode . olivetti-mode)
         (org-mode . olivetti-mode))
  :init
  (+set-defaults!
   olivetti-body-width 120))

;; Misc
(+set-defaults!
 outline-minor-mode-highlight 'override
 outline-minor-mode-cycle t
 outline-minor-mode-use-buttons nil)

;;;; Markdown mode
(use-package markdown-mode
  :init
  (+set-defaults!
   markdown-fontify-code-blocks-natively t))

(defun +flymake-markdown-mode-setup ()
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-markdownlint nil t)
  (flymake-mode))

(add-hook 'text-mode-hook #'+flymake-markdown-mode-setup)

(provide '+writing)
;;; +writing.el ends here
