;;; +term.el --- summary -*- lexical-binding: t -*-

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
(use-package vterm
  :hook
  (vterm-mode . (lambda ()
                  (setq-local buffer-face-mode-face '+vterm-font)
                  (buffer-face-mode)))
  :init
  (+set-defaults! vterm-max-scrollback 50000
                  vterm-kill-buffer-on-exit t)
  (defface +vterm-font '((t)) "Font for vterm" :group 'vterm)
  :config
  (+after-gui!
    (set-face-attribute '+vterm-font nil :fontset "fontset-nerdfont")))

(use-package multi-vterm)

(use-package eat
  :config
  (+after-gui!
    (set-face-attribute 'eat-term-font-default nil :fontset "fontset-nerdfont")))


;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)

;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)


(provide '+term)

;;; +term.el ends here
