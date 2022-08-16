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

(+set-defaults!
 split-width-threshold 120)

(+install! ace-window)

(global-set-key [remap other-window] #'ace-window)

(+install! popper)

(+define-keys! popper
  (popper-mode-map
   (((kbd "C-`") #'popper-toggle-latest)
    ((kbd "M-`") #'popper-cycle)
    ((kbd "C-M-`") #'popper-toggle-type))))

(add-hook 'after-init-hook #'popper-mode)

(defun +popper-group-by-project ()
  (when (featurep 'project)
    (popper-group-by-project)))

(+set-defaults!
 popper-display-control nil
 popper-echo-dispatch-actions t
 popper-group-function #'+popper-group-by-project
 popper-reference-buffers
 '("\\`\\*\\(Messages\\|Backtrace\\|Warnings\\|Output\\|Flymake\\|eldoc\\|Help\\)'"
   "\\`\\*Embark"
   "\\`\\*helpful")
 display-buffer-base-action
   '((display-buffer-reuse-window display-buffer-same-window)
     (reusable-frames . t))
 even-window-sizes nil)

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Messages\\|Backtrace\\|Warnings\\|Output\\|Flymake\\|eldoc\\|Help\\)"
               (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 16)))

;;;; Theme
(+install! modus-themes)

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

(load-theme 'modus-operandi t)

(global-set-key [f10] #'modus-themes-toggle)

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
