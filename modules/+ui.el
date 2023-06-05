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

(use-package darkman
  :ghook ('after-init-hook #'darkman-mode)
  :init
  (general-setq darkman-themes '(:light modus-operandi-tinted :dark modus-vivendi-tinted)))

;;;; Window management

(use-package ace-window)

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

(use-package popper)

(general-define-key
 :keymaps 'popper-mode-map
 "C-`" #'popper-toggle-latest
 "C-~" #'popper-kill-latest-popup
 "M-`" #'popper-cycle
 "C-M-`" #'popper-toggle-type)

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
(use-package nerd-icons
  :demand t)

;;;; Tabs

(defun +tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  (let* ((icon (nerd-icons-sucicon "nf-custom-emacs"))
         (props (get-text-property 0 'face icon)))
    (cl-destructuring-bind (&key family &allow-other-keys) props
      (let ((icon (propertize icon 'face `( :inherit tab-bar
                                            :family ,family))))
        `((menu-bar menu-item ,(concat " " icon " ")
              tab-bar-menu-bar :help "Menu Bar"))))))

(defun +tab-bar-tab-name-format-comfortable (tab i)
  (propertize (concat " " (tab-bar-tab-name-format-default tab i) " ")
              'face (funcall tab-bar-tab-face-function tab)))

(+set-defaults!
 tab-bar-tab-name-format-function #'+tab-bar-tab-name-format-comfortable
 tab-bar-format '(+tab-bar-format-menu-bar
                  tab-bar-format-history
                  tab-bar-format-tabs
                  tab-bar-separator
                  tab-bar-format-add-tab)
 tab-bar-close-button-show nil
 tab-bar-tab-name-truncated-max 14
 tab-bar-new-tab-choice #'+go-to-dashboard)

(tab-bar-mode)
(tab-bar-history-mode)

;;;; Theme
(use-package modus-themes)
(use-package ef-themes)
;; workaround undeclared 'ef-light-palette issue for `ef-themes-with-colors'
(require 'ef-light-theme)
(require 'modus-themes)

(setq
 modus-themes-italic-constructs t
 modus-themes-bold-constructs t
 modus-themes-prompts '(background)
 modus-themes-mixed-fonts t
 modus-themes-org-blocks 'gray-background
 modus-themes-headings '((0 . (2.0))
                         (1 . (rainbow background overline 1.5))
                         (2 . (background overline 1.4))
                         (3 . (background overline 1.3))
                         (4 . (background overline 1.2))
                         (5 . (overline 1.2))
                         (t . (no-bold 1.1)))
 modus-themes-common-palette-overrides
      `((border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)
        ,@modus-themes-preset-overrides-faint
        (builtin magenta)
        (comment fg-dim)
        (constant magenta-cooler)
        (docstring magenta-faint)
        (docmarkup green-faint)
        (fnname magenta-warmer)
        (keyword cyan)
        (preprocessor cyan-cooler)
        (string red-cooler)
        (type magenta-cooler)
        (variable blue-warmer)
        (rx-construct magenta-warmer)
        (rx-backslash blue-cooler))
 x-gtk-use-system-tooltips nil
 tooltip-frame-parameters '((name . "tooltip")
                            (internal-border-width . 3)
                            (border-width . 0)))

(defvar +after-load-theme-hook nil)
(advice-add 'load-theme :after (lambda (&rest _) (run-hooks '+after-load-theme-hook)))
(let ((hook (if (daemonp)
                'server-after-make-frame-hook
              'after-init-hook)))
  (add-hook hook (lambda () (run-hooks '+after-load-theme-hook))))

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
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :init
  (+set-defaults!
   doom-modeline-height 15
   doom-modeline-bar-width 6
   doom-modeline-buffer-encoding 'nondefault
   doom-modeline-buffer-file-name-style 'truncate-upto-project))

(add-hook 'after-init-hook #'size-indication-mode)
(add-hook 'after-init-hook #'column-number-mode)

;;;; Current line highlighting
(use-package pulsar)

(add-hook 'after-init-hook #'pulsar-global-mode)

(use-package lin)

(add-hook 'after-init-hook #'lin-global-mode)

(use-package hyperbole :disabled)

;;;; Dashboard

(use-package dashboard)

(defun +go-to-dashboard ()
  (get-buffer-create "*dashboard*"))

(+set-defaults!
 initial-buffer-choice #'+go-to-dashboard
 dashboard-projects-backend 'project-el
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

(use-package nerd-icons-completion
  :ghook ('marginalia-mode-hook #'nerd-icons-completion-mode))

(use-package kind-icon
  :after corfu
  :demand t
  :init
  (general-setq
   kind-icon-default-face 'corfu-default ;; to compute blended backgrounds correctly
   kind-icon-use-icons nil
   kind-icon-mapping
   `(
     (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
     (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
     (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
     (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
     (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
     (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
     (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
     (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
     (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
     (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
     (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
     (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
     (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
     (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
     (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
     (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
     (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
     (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
     (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
     (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
     (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
     (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
     (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
     (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
     (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
     (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
     (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
     (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
     (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
     (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
     (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
     (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
     (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
     (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
     (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
     (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide '+ui)
;;; +ui.el ends here
