;;; +magit.el --- summary -*- lexical-binding: t -*-

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

(+install! magit)
(+install! magit-tbdiff)
(+install! magit-lfs)

(defun +magit-display-buffer (buffer)
  (let* ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ((memq buffer-mode '(magit-status-mode magit-log-mode magit-log-select-mode))
              `((display-buffer-reuse-window display-buffer-reuse-mode-window +display-buffer-in-direction)
                (direction . leftmost)
                (window-width . 90)))
             ;; Any magit buffers opened from a commit window should open below
             ;; it. Also open magit process windows below.
             ((or (bound-and-true-p git-commit-mode)
                  (eq buffer-mode 'magit-process-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode)
                              0.35
                            0.6)))
                `((display-buffer-reuse-window display-buffer-below-selected)
                  (inhibit-same-window . t)
                  (window-height . ,(truncate (* (window-height) size))))))

             ((or (not (derived-mode-p 'magit-mode))
                  (memq (with-current-buffer buffer major-mode)
                             '(magit-revision-mode
                               magit-diff-mode
                               magit-stash-mode
                               magit-status-mode)))
              '((display-buffer-reuse-window display-buffer-reuse-mode-window +display-buffer-in-direction)
                (direction . right)))

             (t
              '((display-buffer-in-previous-window display-buffer-reuse-mode-window display-buffer-below-selected)
                (window-height . 0.6)))))))

(add-hook 'git-commit-setup-hook #'+magit--git-commit-setup  nil t)

(defun +magit--git-commit-setup ()
  (add-hook 'with-editor-post-finish-hook #'+magit--kill-diff-buffers-in-current-repo))

(defun +magit--kill-diff-buffers-in-current-repo ()
  "Delete the `magit-diff' buffer related to the current repository."
  (let (found)
    (dolist (mode '(magit-diff-mode magit-revision-mode) found)
      (when-let* ((buffer (magit-get-mode-buffer mode))
                  (window (get-buffer-window buffer)))
        (when (window-live-p window)
          (quit-window 1))
        (push buffer found)))
    (when found
      (+magit--kill-diff-buffers-in-current-repo))))

(+set-defaults!
 magit-define-global-key-bindings nil
 magit-save-repository-buffers nil
 magit-diff-refine-hunk t
 magit-diff-refine-hunk t
 magit-display-buffer-function #'+magit-display-buffer
 magit-bury-buffer-function #'magit-restore-window-configuration)

(defvar +magit-map (make-keymap))

(general-define-key
 :keymaps '+magit-map
 "g" #'magit-status
 "l" #'magit-log
 "b" #'magit-blame)

(+after! meow
  (meow-leader-define-key (cons "g" +magit-map)))

(+after! magit
  (transient-append-suffix 'magit-pull "-r"
  '("-a" "Autostash" "--autostash")))

(+after! project
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

(+after! smerge-mode
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map 'smerge-basic-map)))
   smerge-basic-map))

(provide '+magit)

;;; +magit.el ends here
