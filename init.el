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
(require 'packages)

;;;; To organize runtime and config files

(+install! no-littering)

(defvar no-littering-etc-directory)
(defvar no-littering-var-directory)

(require 'xdg)
(setq user-emacs-directory (expand-file-name "emacs" (xdg-config-home))
      no-littering-var-directory (expand-file-name "emacs" (xdg-data-home))
      no-littering-etc-directory (expand-file-name "emacs" (xdg-config-home)))

(setq custom-file (expand-file-name "settings.el" no-littering-etc-directory))

(when (not (bound-and-true-p byte-compile-current-file))
  (startup-redirect-eln-cache (expand-file-name "eln-cache" no-littering-var-directory))
  (require 'no-littering))

(load custom-file 'noerror)

;;;; Theme
(set-defaults
 '(modus-themes-italic-constructs t)
 '(modus-themes-bold-constructs t)
 '(modus-themes-mode-line '(borderless accented))
 '(modus-themes-tabs-accented t)
 '(modus-themes-prompts '(background))
 '(modus-themes-region '(accented bg-only no-extend))
 '(modus-themes-mixed-fonts t)
 '(modus-themes-org-blocks 'gray-background)
 '(modus-themes-syntax '(faint alt-syntax))
 '(modus-themes-lang-checkers '(straight-underline background))
 '(modus-themes-headings '((1 . (rainbow background overline))
                           (2 . (background overline))
                           (3 . (background overline))
                           (4 . (background overline))
                           (5 . (overline))
                           (t . (no-bold))))
 '(modus-themes-scale-headings t)
 '(modus-themes-scale-1 1.2)
 '(modus-themes-scale-2 1.3)
 '(modus-themes-scale-3 1.4)
 '(modus-themes-scale-4 1.5)
 '(modus-themes-scale-title 2.0))

(load-theme 'modus-operandi)

(declare-function modus-themes-toggle "modus-themes")
(global-set-key [f10] #'modus-themes-toggle)

;;;; Modeline

(+install! doom-modeline)
(+install! all-the-icons)
(add-hook 'after-init-hook #'doom-modeline-mode)
(set-defaults
 '(doom-modeline-height 15)
 '(doom-modeline-bar-width 6)
 '(doom-modeline-buffer-encoding 'nondefault)
 '(doom-modeline-buffer-file-name-style 'truncate-upto-project))

(add-hook 'after-init-hook #'size-indication-mode)
(add-hook 'after-init-hook #'column-number-mode)

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

;;;; Fonts
(set-face-attribute 'default nil :font (font-spec :name "Iosevka Comfy" :size 14))
(set-face-attribute 'variable-pitch nil :font (font-spec :name "Iosevka Comfy Duo" :size 14))
;;(set-face-attribute 'fixed-pitch nil :font (font-spec :name "Iosevka Comfy" :size 14))

(+install! mixed-pitch)
(add-hook 'text-mode-hook #'mixed-pitch-mode)

;;;; Editing

(+install! meow)
(require 'meow)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
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
   '("?" . meow-cheatsheet))
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

(meow-setup)
(meow-global-mode)

;;;;; Undo management

(+install! vundo)

(meow-leader-define-key
 '("u" . vundo))

;;;; Completion

(+install! vertico)
(add-hook 'after-init-hook #'vertico-mode)
(with-eval-after-load 'vertico
  (defvar vertico-map)
  (define-key vertico-map "\r" #'vertico-directory-enter)
  (define-key vertico-map "\d" #'vertico-directory-delete-char)
  (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (define-key vertico-map "\M-h" #'vertico-directory-up))

(set-defaults
 '(vertico-cycle t))

(+install! marginalia)
(add-hook 'after-init-hook #'marginalia-mode)
(define-key minibuffer-local-map "\M-A" #'marginalia-cycle)

(+install! consult)
(set-defaults
 '(corfy-cycle t)
 '(corfu-echo-documentation t))


(+install! orderless)

(defun +orderless--suffix-regexp ()
  (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
      (format "[%c-%c]*$"
              consult--tofu-char
              (+ consult--tofu-char consult--tofu-range -1))
    "$"))

(eval-when-compile (require 'orderless))
(defvar +orderless-dispatch-alist
  '((?% . char-fold-to-regexp)
    (?! . orderless-without-literal)
    (?` . orderless-initialism)
    (?= . orderless-literal)
    (?~ . orderless-flex)))

;; Recognizes the following patterns:
;; * ~flex flex~
;; * =literal literal=
;; * %char-fold char-fold%
;; * `initialism initialism`
;; * !without-literal without-literal!
;; * .ext (file extension)
;; * regexp$ (regexp matching at end)
(defun +orderless-dispatch (word _index _total)
  (cond
   ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
   ((string-suffix-p "$" word)
    `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--suffix-regexp))))
   ;; File extensions
   ((and (or minibuffer-completing-file-name
             (derived-mode-p 'eshell-mode))
         (string-match-p "\\`\\.." word))
    `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--suffix-regexp))))
   ;; Ignore single !
   ((equal "!" word) `(orderless-literal . ""))
   ;; Prefix and suffix
   ((if-let (x (assq (aref word 0) +orderless-dispatch-alist))
        (cons (cdr x) (substring word 1))
      (when-let (x (assq (aref word (1- (length word))) +orderless-dispatch-alist))
        (cons (cdr x) (substring word 0 -1)))))))

;; Define orderless style with initialism by default
(orderless-define-completion-style +orderless-with-initialism
  (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

;; You may want to combine the `orderless` style with `substring` and/or `basic`.
;; There are many details to consider, but the following configurations all work well.
;; Personally I (@minad) use option 3 currently. Also note that you may want to configure
;; special styles for special completion categories, e.g., partial-completion for files.
;;
;; 1. (setq completion-styles '(orderless))
;; This configuration results in a very coherent completion experience,
;; since orderless is used always and exclusively. But it may not work
;; in all scenarios. Prefix expansion with TAB is not possible.
;;
;; 2. (setq completion-styles '(substring orderless))
;; By trying substring before orderless, TAB expansion is possible.
;; The downside is that you can observe the switch from substring to orderless
;; during completion, less coherent.
;;
;; 3. (setq completion-styles '(orderless basic))
;; Certain dynamic completion tables (completion-table-dynamic)
;; do not work properly with orderless. One can add basic as a fallback.
;; Basic will only be used when orderless fails, which happens only for
;; these special tables.
;;
;; 4. (setq completion-styles '(substring orderless basic))
;; Combine substring, orderless and basic.
;;
(declare-function orderless-escapable-split-on-space "orderless")
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
          ;;; Enable partial-completion for files.
          ;;; Either give orderless precedence or partial-completion.
          ;;; Note that completion-category-overrides is not really an override,
          ;;; but rather prepended to the default completion-styles.
      ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
      completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                      ;; enable initialism by default for symbols
                                      (command (styles +orderless-with-initialism))
                                      (variable (styles +orderless-with-initialism))
                                      (symbol (styles +orderless-with-initialism)))
      orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
      orderless-style-dispatchers '(+orderless-dispatch))


(+install! embark)
(+install! embark-consult)
(global-set-key [remap describe-bindings] #'embark-bindings)
(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "C-;") #'embark-dwim)

                (setq prefix-help-command #'embark-prefix-help-command)
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(with-eval-after-load 'embark
(require 'embark-consult)
(declare-function consult-preview-at-point-mode "consult")
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(+install! corfu)
(+install! corfu-doc)

(set-defaults
 '(corfu-cycle t)
 '(corfu-echo-documentation 0.3))

(global-corfu-mode 1)
(add-hook 'eshell-mode-hook
          (lambda () (setq-local corfu-quit-at-boundary t
                                 corfu-quit-no-match t)
            (corfu-mode)))

(defun +corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (bound-and-true-p vertico--input)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'+corfu-enable-always-in-minibuffer 1)

(defvar corfu--extra)

(defun +corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))
(with-eval-after-load 'corfu
  (defvar corfu-map)
  (add-hook 'corfu-mode-hook #'corfu-doc-mode)
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)
  (define-key corfu-map (kbd "M-m") #'+corfu-move-to-minibuffer))

(+install! cape)

(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; Silence the pcomplete capf, no errors or messages!
;; Important for corfu
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

;;;; Programming

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; init.el ends here
