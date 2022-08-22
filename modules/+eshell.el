;;; +eshell.el --- summary -*- lexical-binding: t -*-

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
(+install! vterm)
(+install! eshell-vterm)
(+install! eshell-syntax-highlighting)
(+install! eshell-fringe-status)
(+install! eshell-prompt-extras)

(add-hook 'eshell-mode-hook #'eshell-fringe-status-mode)
(add-hook 'eshell-mode-hook #'eshell-syntax-highlighting-mode)
(add-hook 'after-init-hook #'eshell-vterm-mode)

(add-to-list 'display-buffer-alist
             '("\\`\\*eshell\\*\\'"
               (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-at-bottom)
               (window-height . 0.3)
               (dedicated . t)))

(+set-defaults! eshell-where-to-jump 'begin
                eshell-review-quick-commands nil
                eshell-smart-space-goes-to-end t)

(+after! eshell
  (add-to-list 'eshell-modules-list 'eshell-smart))

(+after! esh-opt
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(+after! em-prompt
  (add-hook 'eshell-mode-hook (lambda () (setq outline-regexp eshell-prompt-regexp))))

(+after! em-term
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
  (add-to-list 'eshell-visual-subcommands '("git" "log" "lol" "diff" "reflog" "show")))

(defun eshell/rg (&rest args)
  "Use rg in eshell"
  (require 'em-unix)
  (eshell-grep "rg" args t))

(defun eshell/z (&optional regexp)
  "Navigate to a previously visited directory in eshell, or to
  any directory proferred by `consult-dir'."
  (require 'em-dirs)
  (require 'consult-dir)
  (let ((eshell-dirs (delete-dups
                      (mapcar 'abbreviate-file-name
                              (ring-elements eshell-last-dir-ring)))))
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                 :narrow ?e
                                                 :category file
                                                 :face consult-file
                                                 :items ,eshell-dirs))
             (consult-dir-sources (cons consult-dir--source-eshell
                                        consult-dir-sources)))
        (eshell/cd (substring-no-properties
                    (consult-dir--pick "Switch directory: ")))))
     (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                     (completing-read "cd: " eshell-dirs)))))))

(defalias 'eshell/v 'eshell-exec-visual)

(provide '+eshell)

;;; +eshell.el ends here
