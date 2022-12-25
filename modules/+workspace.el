;;; +workspace.el --- persistent workspaces -*- lexical-binding: t -*-

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
(+install! persp-mode)
(+install! persp-mode-project-bridge)

(+set-defaults!
 persp-nil-name "None"
 persp-autokill-buffer-on-remove 'kill
 persp-set-last-persp-for-new-frames t
 persp-keymap-prefix (kbd "C-c C-p")
 persp-mode-project-bridge-persp-name-prefix "🅿 ")

(add-hook 'window-setup-hook (lambda ()
                               (persp-mode 1)
                               (persp-mode-project-bridge-mode 1)))

(add-hook 'persp-before-deactivate-functions
          (defun +workspaces-save-tab-bar-data-h (_)
            (when (get-current-persp)
              (set-persp-parameter
               'tab-bar-tabs (tab-bar-tabs)))))

(add-hook 'persp-activated-functions
          (defun +workspaces-load-tab-bar-data-h (_)
            (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
            (tab-bar--update-tab-bar-lines t)))

(add-hook 'persp-before-save-state-to-file-functions
          (defun +workspaces-save-tab-bar-data-to-file-h (&rest _)
            (when (get-current-persp)
              (set-persp-parameter 'tab-bar-tabs (frameset-filter-tabs (tab-bar-tabs) nil nil t)))))

(add-hook 'persp-after-load-state-functions
          (defun +workspaces-load-state-h (&rest _)
              (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
              (tab-bar--update-tab-bar-lines t)))

(advice-add 'persp-mode-project-bridge-hook-switch
            :after (defun +persp-mode-project-bridge-hook-switch-advice(&rest args)
                     (set-persp-parameter 'dont-save-to-file nil (get-current-persp))))

(+after! consult
      (defvar consult--source-persp-mode
        `( :name "Perspective"
           :narrow   ?s
           :category buffer
           :face     consult-buffer
           :history  buffer-name-history
           :state    ,#'consult--buffer-state
           :default  t
           :items
           ,(lambda ()
              (with-persp-buffer-list () (consult--buffer-query :sort 'visibility
                                                                :as #'buffer-name)))))

      (push consult--source-persp-mode consult-buffer-sources))

(add-hook 'persp-mode-project-bridge-mode
          (lambda ()
            (if persp-mode-project-bridge-mode
                (persp-mode-project-bridge-find-perspectives-for-all-buffers)
              (persp-mode-project-bridge-kill-perspectives))))


(provide '+workspace)

;;; +workspace.el ends here
