;;; +editing.el --- Configuration related to code and text editing -*- lexical-binding: t -*-

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
(+install! meow)
(require 'meow)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-keypad-ctrl-meta-prefix ?n)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   (cons "p" project-prefix-map))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(when window-system
  (setq meow-replace-state-name-list
        '((normal . "🅽")
          (beacon . "🅱")
          (insert . "🅸")
          (motion . "🅼")
          (keypad . "🅺"))))

(meow-setup)
(meow-global-mode)

(+install! wgrep)

(+install! repeat-help)
(+set-defaults! repeat-help-popup-type 'embark)
(add-hook 'after-init-hook #'repeat-mode)
(add-hook 'repeat-mode-hook #'repeat-help-mode)
(add-hook 'repeat-help-mode-hook (lambda ()
  ;; Restore non-intrusive help as well as verbose prompt
  (setq repeat-echo-function repeat-help--echo-function)))

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Backups and auto-save
(+set-defaults! backup-by-copying t
                version-control t
                delete-old-versions t
                kept-new-versions 6
                kept-old-versions 3)

;; Whitespace
(+set-defaults! whitespace-style
                '(face empty trailing tab-mark
                       indentation::space))
(customize-set-variable 'whitespace-action '(cleanup auto-cleanup))
(add-hook 'prog-mode-hook #'whitespace-mode)

(defun +setup-whitespace-mode ()
  "Whitespace mode for text mode buffers, except Magit.

Whitespace mode somehow breaks message box when using native
compilation via clearing the first two emtpy lines."
  (unless (bound-and-true-p git-commit-mode)
    (whitespace-mode)))
(add-hook 'text-mode-hook #'+setup-whitespace-mode)

;;;; Undo management

(+install! vundo)
(require 'vundo)
(meow-leader-define-key
 '("u" . vundo))

(+set-defaults!
 ;; based on undo-tree's limits
 undo-limit (* 80 1024 1024)
 undo-strong-limit (* 120 1024 1024)
 undo-strong-limit (* 360 1024 1024))

(+install! undohist)
(add-hook 'after-init-hook #'undohist-initialize)

;;;; Templating
(+install! tempel)

(defun +tempel-setup-capf ()
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))

(add-hook 'prog-mode-hook #'+tempel-setup-capf)
(add-hook 'text-mode-hook #'+tempel-setup-capf)

;; Requires no-littering
(unless (bound-and-true-p byte-compile-current-file)
  (require 'no-littering)
  (+set-defaults! tempel-path (list (no-littering-expand-var-file-name "templates")
                                    (expand-file-name "templates/*.eld" +emacs-config-root))
                  auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(provide '+editing)
;;; +editing.el ends here
