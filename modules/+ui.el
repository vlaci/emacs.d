;;; +ui.el --- summary -*- lexical-binding: t -*-

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

;;;; Window management

(+install! ace-window)

(+set-defaults! av-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(global-set-key (kbd "M-o") #'ace-window)

(global-set-key [remap kill-buffer] #'kill-this-buffer)

(+set-defaults!
 pgtk-wait-for-event-timeout 0.001
 window-combination-resize t
 display-buffer-base-action '((display-buffer-reuse-window display-buffer-same-window)
                              (reusable-frames . t))
 even-window-sizes nil
 split-height-threshold nil
 split-width-threshold 200)

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Messages\\|Backtrace\\|Warnings\\|Output\\|Flymake\\|eldoc\\|Help\\)"
               (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 16)))
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(rustic\\|carg\\)"
               (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.4)))
(add-to-list 'display-buffer-alist
             '("\\*\\(Calendar\\|Bookmark Annotation\\).*"
               (display-buffer-reuse-mode-window display-buffer-below-selected)
               (window-height . fit-window-to-buffer)))

(+install! popper)

(+define-keys! popper
  (popper-mode-map
   ((kbd "C-`") #'popper-toggle-latest)
   ((kbd "C-~") #'popper-kill-latest-popup)
   ((kbd "M-`") #'popper-cycle)
   ((kbd "C-M-`") #'popper-toggle-type)))

(add-hook 'after-init-hook #'popper-mode)

(+after! popper-echo
  (popper-echo-mode))

(defun +popper-group-by-project ()
  (when (featurep 'project)
    (popper-group-by-project)))

(+set-defaults!
 popper-display-control nil
 popper-echo-dispatch-actions t
 popper-group-function #'+popper-group-by-project
 popper-reference-buffers
 '("\\`\\*\\(Messages\\|Backtrace\\|Warnings\\|Output\\|Flymake\\|eldoc\\|Help\\)\\*\\'"
   "\\`\\*Embark"
   "\\`\\*eshell\\*\\'"
   "\\`\\*\\(rustic\\|cargo\\)"
   "\\`\\*helpful"))

;;;; Assets
(+install! all-the-icons)
(autoload #'all-the-icons-fileicon "all-the-icons")

;;;; Tabs

(defun +tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  (let* ((icon (all-the-icons-fileicon "emacs"))
         (props (get-text-property 0 'face icon)))
    (cl-destructuring-bind (&key family &allow-other-keys) props
      (let ((icon (propertize (concat " " icon " ") 'face `( :inherit tab-bar
                                                             :family ,family))))
        `((menu-bar menu-item ,icon
              tab-bar-menu-bar :help "Menu Bar"))))))

(defun +tab-bar-tab-name-format-comfortable (tab i)
  (propertize (concat " " (tab-bar-tab-name-format-default tab i) " ")
              'face (funcall tab-bar-tab-face-function tab)))

(declare-function project-root "project")

(defun +tab-bar-tab-name ()
  "Use project/directory as tab name."
  (let ((dir (if-let ((proj (project-current)))
                 (project-root proj)
               default-directory)))
    (file-name-base (directory-file-name dir))))

(+set-defaults!
 tab-bar-tab-name-format-function #'+tab-bar-tab-name-format-comfortable
 tab-bar-format '(+tab-bar-format-menu-bar
                  tab-bar-format-history
                  tab-bar-format-tabs
                  tab-bar-separator
                  tab-bar-format-add-tab)
 tab-bar-close-button-show nil
 tab-bar-tab-name-truncated-max 14
 tab-bar-new-tab-choice #'+go-to-dashboard
 tab-bar-tab-name-function #'+tab-bar-tab-name)

(tab-bar-mode)
(tab-bar-history-mode)

;;;; Theme
(+install! modus-themes)
(+install! ef-themes)
;; workaround undeclared 'ef-light-palette issue for `ef-themes-with-colors'
(require 'ef-light-theme)

(+set-defaults!
 modus-themes-italic-constructs t
 modus-themes-bold-constructs t
 modus-themes-mode-line '(borderless)
 modus-themes-tabs-accented t
 modus-themes-prompts '(background)
 modus-themes-region '(accented bg-only no-extend)
 modus-themes-mixed-fonts t
 modus-themes-org-blocks 'gray-background
 modus-themes-syntax '(faint alt-syntax)
 modus-themes-lang-checkers '(straight-underline background)
 modus-themes-headings '((1 . (rainbow background overline))
                         (2 . (background overline))
                         (3 . (background overline))
                         (4 . (background overline))
                         (5 . (overline))
                         (t . (no-bold)))
 modus-themes-scale-headings t
 modus-themes-scale-1 1.2
 modus-themes-scale-2 1.3
 modus-themes-scale-3 1.4
 modus-themes-scale-4 1.5
 modus-themes-scale-title 2.0
 x-gtk-use-system-tooltips nil
 tooltip-frame-parameters '((name . "tooltip")
                            (internal-border-width . 3)
                            (border-width . 0)))

(defvar +after-load-theme-hook nil)
(advice-add 'load-theme :after (lambda (&rest _) (run-hooks '+after-load-theme-hook)))

(defmacro +meow-prot-themes-custom-faces (theme)
  "Common face override for `modus' and `ef' THEMEs."
  (let ((theme-name (symbol-name theme)))
    `(defun ,(intern (concat "+meow-" theme-name "-custom-faces")) ()
       (+after! meow
         (eval
          '(,(intern (concat theme-name "-with-colors"))
            (set-face-attribute 'meow-beacon-indicator nil :foreground magenta)
            (set-face-attribute 'meow-insert-indicator nil :foreground red)
            (set-face-attribute 'meow-keypad-indicator nil :foreground cyan)
            (set-face-attribute 'meow-motion-indicator nil :foreground fg-dim)
            (set-face-attribute 'meow-normal-indicator nil :foreground blue)))))))

(add-hook 'modus-themes-after-load-theme-hook (+meow-prot-themes-custom-faces modus-themes))
(add-hook 'ef-themes-post-load-hook (+meow-prot-themes-custom-faces ef-themes))

(add-hook '+after-load-theme-hook #'+customize-prot-theme)

(defun +customize-prot-theme (&rest _)
  (cond ((and (fboundp 'modus-themes--current-theme) (modus-themes--current-theme)) (run-hooks 'modus-themes-after-load-theme-hook))
        ((and (fboundp 'ef-themes--current-theme) (ef-themes--current-theme)) (run-hooks 'ef-themes-post-load-hook))))

;; Change theme interactively

(global-set-key [f10] #'+theme-toggle)

(defun +theme-toggle (&optional force)
  "Toggle between modus or ef themes or FORCE theme selection."
  (interactive "P")
  (cond (force (call-interactively 'consult-theme))
        ((and (fboundp 'modus-themes--current-theme) (modus-themes--current-theme)) (modus-themes-toggle))
        ((and (fboundp 'ef-themes--current-theme) (ef-themes--current-theme)) (call-interactively 'ef-themes-select nil))
        (t (call-interactively 'consult-theme))))

;; Persist applied theme

(defun +save-current-theme ()
  (customize-save-variable 'custom-enabled-themes custom-enabled-themes))

(add-hook 'after-init-hook (lambda ()
                             (unless custom-enabled-themes
                               (load-theme 'modus-operandi :no-confirm))
                             (add-hook '+after-load-theme-hook #'+save-current-theme)))

(advice-add 'consult-theme :around (lambda (&rest args)
                                     (cl-letf (((symbol-function '+save-current-theme) #'ignore))
                                       (apply args))
                                     (+save-current-theme)))

;;;; Modeline
(+install! doom-modeline)
(add-hook 'after-init-hook #'doom-modeline-mode)
(+set-defaults!
 doom-modeline-height 15
 doom-modeline-bar-width 6
 doom-modeline-buffer-encoding 'nondefault
 doom-modeline-buffer-file-name-style 'truncate-upto-project)

(add-hook 'after-init-hook #'size-indication-mode)
(add-hook 'after-init-hook #'column-number-mode)

;;;; Current line highlighting
(+install! pulsar)

(add-hook 'after-init-hook #'pulsar-global-mode)

(+install! lin)

(add-hook 'after-init-hook #'lin-global-mode)

;;;; Sideline
(+install! sideline)
(+install! sideline-flymake)

(+set-defaults! sideline-backends-skip-current-line nil
                sideline-backends-right '(sideline-flymake))

(add-hook 'prog-mode-hook #'sideline-mode)

(+install! hyperbole 'no-require)

;;;; Dashboard

(+install! dashboard)

(defun +go-to-dashboard ()
  (get-buffer-create "*dashboard*"))

(+set-defaults!
 initial-buffer-choice #'+go-to-dashboard
 dashboard-center-content t
 dashboard-set-heading-icons t
 dashboard-set-file-icons t
 dashboard-set-navigator t
 dashboard-banner-logo-title ""
 dashboard-startup-banner (expand-file-name "logo.png" +emacs-config-root)
 dashboard-items '((recents  . 5)
                   (projects . 5)
                   (bookmarks . 5)
                   (agenda . 5)))

(dashboard-setup-startup-hook)

(provide '+ui)
;;; +ui.el ends here
