;;; init.el --- summary -*- lexical-binding: t -*-

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

(+install! gcmh)
(gcmh-mode)

;;;; To organize runtime and config files

(+install! no-littering 'no-require)

(setq custom-file (expand-file-name "etc/settings.el" user-emacs-directory))
(add-hook 'after-init-hook (lambda () (load custom-file 'noerror)))
(load "nix-integration" 'noerror)

(when (not (bound-and-true-p byte-compile-current-file))
  (defvar no-littering-etc-directory)
  (defvar no-littering-var-directory)
  (require 'no-littering))

;;; Defaults
(+set-defaults!
 use-short-answers t
 vc-follow-symlinks t
 mouse-yank-at-point t
 indent-tabs-mode nil
 tab-width 4
 global-auto-revert-non-file-buffers t  ;; for dired et al
 recentf-max-saved-items 1000
 kill-do-not-save-duplicates t
 auto-window-vscroll nil
 fast-but-imprecise-scrolling t
 scroll-conservatively 101
 scroll-margin 0
 scroll-preserve-screen-position t
 savehist-save-minibuffer-history t
 history-length 1000
 history-delete-duplicates t
 auto-revert-verbose nil
 show-paren-context-when-offscreen 'child-frame
 read-process-output-max (* 1024 1024))

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

(add-hook 'after-init-hook #'recentf-mode)

(add-hook 'recentf-load-hook (lambda()
  (defvar recentf-exclude)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude user-emacs-directory)
  (run-at-time nil (* 5 60) 'recentf-save-list)))

(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Enable savehist-mode for an command history
(savehist-mode 1)

(require '+ui)

;;;; To improve help
(+install! helpful)
(+install! elisp-demos)

(global-set-key [remap describe-command] #'helpful-command)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-macro] #'helpful-macro)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key [remap describe-symbol] #'helpful-symbol)
(global-set-key [remap describe-variable] #'helpful-variable)

(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
(add-hook 'helpful-mode-hook #'visual-line-mode)

(add-to-list 'display-buffer-alist
             '("\\`\\*helpful"
               (display-buffer-in-side-window display-buffer-reuse-mode-window)
               (side . right)
               (slot . 0)
               (window-width . 0.3)))

;;;; Fonts
(set-face-attribute 'default nil :font (font-spec :name "Iosevka Comfy" :size 14))
(set-face-attribute 'variable-pitch nil :font (font-spec :name "Iosevka Comfy Duo" :size 14))
;;(set-face-attribute 'fixed-pitch nil :font (font-spec :name "Iosevka Comfy" :size 14))

(+install! ligature)

(+after! ligature
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))

(add-hook 'after-init-hook #'global-ligature-mode)

(+install! mixed-pitch)
(add-hook 'text-mode-hook #'mixed-pitch-mode)

(require '+editing)
(require '+completion)

;;;; Programming
(require '+programming)
(require '+writing)
(require '+magit)
(require '+org)
(require '+dired)

;;; init.el ends here