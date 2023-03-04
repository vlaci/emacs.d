;;; +dired.el --- summary -*- lexical-binding: t -*-

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

(use-package dired-subtree)
(use-package dired-narrow)
(use-package diredfl)
(use-package fd-dired)

(+set-defaults!
  dired-recursive-copies 'always
  dired-recursive-deletes 'always
  dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"
  dired-dwim-target t
  dired-auto-revert-buffer #'dired-directory-changed-p   ; also see `dired-do-revert-buffer'
  dired-mouse-drag-files t  ; Emacs 29.1
  dired-isearch-filenames 'dwim
  dired-create-destination-dirs 'ask
  dired-vc-rename-file t
  dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))
  dired-clean-up-buffers-too t
  dired-clean-confirm-killing-deleted-buffers t
  dired-clean-up-buffers-too t
  dired-clean-confirm-killing-deleted-buffers t)

(+after! dired
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'dired-mode-hook #'diredfl-mode))

(+after! dired-aux
  (add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))

(general-define-key
  :keymaps 'dired-mode-map
  "<tab>" #'dired-subtree-toggle
  "<backtab>" #'dired-subtree-remove
  "l" #'dired-hist-go-back
  "r" #'dired-hist-go-forward
  "/" #'dired-narrow)


(general-define-key [remap find-dired] #'fd-dired)

(add-hook 'after-init #'dired-hist-mode)

(provide '+dired)
;;; +dired.el ends here
