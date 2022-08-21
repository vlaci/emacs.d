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

(global-set-key [remap other-window] #'ace-window)
(global-set-key [remap kill-buffer] #'kill-this-buffer)

(+set-defaults!
 window-combination-resize t
 display-buffer-base-action '((display-buffer-reuse-window display-buffer-same-window)
                              (reusable-frames . t))
 even-window-sizes nil
 split-height-threshold nil
 split-width-threshold 120)

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Messages\\|Backtrace\\|Warnings\\|Output\\|Flymake\\|eldoc\\|Help\\)"
               (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 16)))
(add-to-list 'display-buffer-alist
             '("\\*\\(Calendar\\|Bookmark Annotation\\).*"
               (display-buffer-reuse-mode-window display-buffer-below-selected)
               (window-height . fit-window-to-buffer)))

(+install! popper)

(+define-keys! popper
  (popper-mode-map
   ((kbd "C-`") #'popper-toggle-latest)
   ((kbd "M-`") #'popper-cycle)
   ((kbd "C-M-`") #'popper-toggle-type)))

(add-hook 'after-init-hook #'popper-mode)

(defun +popper-group-by-project ()
  (when (featurep 'project)
    (popper-group-by-project)))

(+set-defaults!
 popper-display-control nil
 popper-echo-dispatch-actions t
 popper-group-function #'+popper-group-by-project
 popper-reference-buffers
 '("\\`\\*\\(Messages\\|Backtrace\\|Warnings\\|Output\\|Flymake\\|eldoc\\|Help\\)\\*'"
   "\\`\\*Embark"
   "\\`\\*helpful"))

(add-hook 'after-init-hook #'winner-mode)

;;;; Theme
(+install! modus-themes)
(+install! ef-themes)

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

(load-theme 'modus-operandi :no-confirm)

(defun +after-load-theme ()
  (cond ((modus-themes--current-theme) (run-hooks 'modus-themes-after-load-theme-hook))
        ((ef-themes--current-theme) (run-hooks 'ef-themes-post-load-hook))))

(+after-load-theme)

(defun +theme-toggle (&optional force)
  "Toggle between modus or ef themes or FORCE theme selection."
  (interactive "P")
  (cond (force (call-interactively 'consult-theme))
        ((modus-themes--current-theme) (modus-themes-toggle))
        ((ef-themes--current-theme) (call-interactively 'ef-themes-select nil))
        (t (call-interactively 'consult-theme))))

(+after! consult
  (defvar +consult-theme-after-load-hook nil)
  (advice-add #'consult-theme :after (lambda () (run-hooks '+consult-theme-after-load-hook)))
  (global-set-key [f10] #'+theme-toggle))

;;;; Modeline
(+install! doom-modeline)
(+install! all-the-icons)
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

(provide '+ui)
;;; +ui.el ends here
