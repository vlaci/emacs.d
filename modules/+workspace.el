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
(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :init
  (+set-defaults! tabspaces-use-filtered-buffers-as-default t
    tabspaces-default-tab "Default"
    tabspaces-remove-to-default t
    tabspaces-include-buffers '("*scratch*")
    ;; sessions
    tabspaces-session t
    tabspaces-session-auto-restore t)

  (+after! consult
  ;; hide full buffer list (still available with "b" prefix)
  ;(consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))

      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(provide '+workspace)

;;; +workspace.el ends here
