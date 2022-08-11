;;; completion.el --- summary -*- lexical-binding: t -*-

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
(require '+packages)

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

(provide '+completion)
;;; +completion.el ends here
