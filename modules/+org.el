;;; +org.el --- summary -*- lexical-binding: t -*-

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

(+install! org)
(+install! org-cliplink)
(+install! org-modern)
(+install! org-appear)
(+install! orgit)
(+install! org-roam)
(+install! org-roam-ui)
(+install! git-auto-commit-mode)

(+set-defaults!
 org-M-RET-may-split-line '((default . nil))
 org-hide-hide-emphasis-markers t
 org-ellipsis "▾"
 org-list-indent-offset 1
 org-catch-invisible-edits 'show
 org-modules nil
 org-make-link-description-function #'+org-url-get-title
 org-modern-star ["›"]
 org-refile-targets '((org-agenda-files . (:maxlevel . 2))
                      (nil . (:maxlevel . 2)))
 org-refile-use-outline-path 'file
 org-refile-allow-creating-parent-nodes 'confirm
 org-outline-path-complete-in-steps t ;; requires advising `org-olpath-completing-read'
 org-todo-keywords
 '((sequence "TODO(t)" "MAYBE(m)" "WAIT(w@/!)" "|" "CANCEL(c@)" "DONE(d!)"))
 org-todo-keyword-faces '(("WAIT" . '(bold org-todo))
                          ("MAYBE" . '(bold shadow))
                          ("CANCEL" . '(bold org-done)))
 org-use-fast-todo-selection 'expert
 org-priority-faces '((?A . '(bold org-priority))
                      (?B . org-priority)
                      (?C . '(shadow org-priority)))
 org-enforce-todo-dependencies t
 org-enforce-todo-checkbox-dependencies t
 org-log-done 'time
 org-log-redeadline 'time
 org-log-reschedule 'time
 org-log-into-drawer t
 org-clock-into-drawer "CLOCK"
 org-read-date-prefer-future 'time
 org-capture-templates
 (let ((personal-todo-file "personal/my-life.org")
       (work-todo-file "work/my-work.org")
       (work-journal-file "work/work-journal.org"))
   (require 'cl-macs)
   (cl-flet ((todo-entry (key file)
                       `(,key "Todo" entry
                             (file+headline ,file "Tasks to be reviewed")
                             ,(concat "* %^{Title}\n"
                                      ":PROPERTIES:\n"
                                      ":CAPTURED: %U\n"
                                      ":END:\n\n"
                                      "%i%l")
                             :empty-lines-after 1))
          (clock-task (key file)
                       `(,key "Clock in to a task" entry
                             (list 'file+headline ,file "Clocked tasks")
                             ,(concat "* TODO %^{Title}\n"
                                      "SCHEDULED: %T\n"
                                      ":PROPERTIES:\n"
                                      ":EFFORT: %^{Effort estimate in minutes|5|10|15|30|45|60|90|120}\n"
                                      ":END:\n\n"
                                      "%a\n")
                             :prepend t
                             :clock-in t
                             :clock-keep t
                             :immediate-finish t
                             :empty-lines-after 1)))
         `(("p" "Personal")
           ,(todo-entry "pt" personal-todo-file)
           ,(clock-task "pc" personal-todo-file)
           ("w" "Work")
           ,(todo-entry "wt" work-todo-file)
           ,(clock-task "wc" work-todo-file)
           ("wd" "Daily stand-up" entry
            (file+olp+datetree ,work-journal-file)
            (file "work/daily-standup.orgcaptpl")
            :empty-lines-after 1)
           ("wm" "Memorandum of conversation" entry
            (file+olp+datetree ,work-journal-file)
            ,(concat "* Memorandum of conversation on %^{Topic}\n"
                     "%i%?")
            :empty-lines-after 1)
           ("wd" "Task with a due date" entry
            (file+headline ,work-todo-file "Tasks with a date")
            ,(concat "* TODO %^{Title} %^g\n"
                     "SCHEDULED: %^t\n"
                     ":PROPERTIES:\n"
                     ":CAPTURED: %U\n"
                     ":END:\n\n"
                     "%a\n%i%?")
            :empty-lines-after 1))))
 org-agenda-restore-windows-after-quit t
 org-roam-v2-ack t)

(+define-keys! org
  (global-map
   ((kbd "C-c a") #'org-agenda)
   ((kbd "C-c c") #'org-capture)
   ((kbd "C-c l") #'org-store-link)))

(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(defun +org-url-get-title (url &optional _)
  "Take an URL and return the value of the <title> HTML tag.
Thanks to https://frozenlock.org/tag/url-retrieve/ for documenting `url-retrieve'."
  (when (string-match "https?://" url)
    (let ((buffer (url-retrieve-synchronously url))
          (title nil))
      (with-current-buffer buffer
        (goto-char (point-min))
        (search-forward-regexp "<title>\\([^<]+?\\)</title>")
        (setq title (url-unhex-string (match-string 1 )) )
        (kill-buffer (current-buffer)))
      title)))

(advice-add #'org-olpath-completing-read :around
            (lambda (&rest args)
              (minibuffer-with-setup-hook
                  (lambda () (setq-local completion-styles '(basic)))
                (apply args))))

(advice-add #'org-capture-fill-template :around
            (lambda (&rest args)
              (let ((default-directory  org-directory))
                (apply args))))

(provide '+org)
;;; +org.el ends here
