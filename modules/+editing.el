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

(use-package evil
  :init
  (+set-defaults!
   evil-want-keybinding nil
   evil-want-C-w-delete t
   evil-want-Y-yank-to-eol t
   evil-ex-visual-char-range t
   evil-symbol-word-search t
   evil-ex-interactive-search-highlight 'selected-window
   evil-search-module 'evil-search
   evil-kbd-macro-supress-motion-error t
   evil-undo-system 'undo-redo
   evil-want-fine-undo t
   evil-visual-state-cursor 'hollow
   evil-visual-update-x-selection-p nil
   evil-move-cursor-back nil
   evil-move-beyond-eol t)

  (evil-mode)
  :config
  ;; use `general-key-dispatch' for "c" (e.g. to bind cx to `evil-exchange')
  (general-def :prefix-map '+c-map
    "c" (general-simulate-key (#'evil-change "c")))
  (general-def 'normal
    "c" (general-key-dispatch #'evil-change
          :inherit-keymap +c-map))
  (general-def 'visual "c" #'evil-change)
  (general-def 'normal "M" #'evil-set-marker)
  (general-rr
    "/" #'evil-ex-search-forward
    "?" #'evil-ex-search-backward))

(use-package evil-collection
  :demand t
  :config
  (dolist (mode evil-collection-mode-list)
    (dolist (req (or (cdr-safe mode) (list mode)))
      (with-eval-after-load req
        (message "Loading evil-collection for mode %s" req)
        (evil-collection-init (list mode)))))

  (evil-collection-init
   '(
     help
     (buff-menu "buff-menu")
     calc
     image
     elisp-mode
     replace
     (indent "indent")
     (process-menu simple)
     shortdoc
     tabulated-list
     tab-bar)))

(use-package wgrep)

(use-package evil-easymotion
 :general
  (general-def 'motion
    "f" #'+sneak-forward-f
    "F" #'+sneak-backward-F)

  (general-r
    "t" #'+sneak-forward-exclusive-t
    "T" #'+sneak-backward-exclusive-T)

  ;; TODO combine next two statements when general supports modifier prefixes
  (general-r
    "n" #'evilem-next-line
    "e" #'evilem-prev-line
    ;; too many candidates to be that useful
    ;; "w" #'evilem-forward-word-begin
    ;; so useful it might even warrant a single letter keybinding (e.g.
    ;; replacing f keybinding)
    "w" #'evil-avy-goto-word-or-subword-1
    "W" #'evilem-forward-WORD-begin
    "b" #'evilem-backward-word-begin
    "B" #'evilem-backward-WORD-begin
    "s" #'evilem-sentence-nav-forward
    "S" #'evilem-sentence-nav-backward
    "p" #'evilem-forward-paragraph
    "P" #'evilem-backward-paragraph)

  ;; can't use control since can't distinguish certain keys (can rebind in
  ;; `input-decode' map to get working, though will only work for GUI)
  (general-def '(insert emacs)
    "M-n" #'evilem-next-line
    "M-e" #'evilem-prev-line
    "M-w" #'evilem-forward-word-begin
    "M-W" #'evilem-forward-WORD-begin
    ;; don't override backward word
    ;; "M-b" #'evilem-backward-word-begin
    "M-B" #'evilem-backward-WORD-begin
    "M-s" #'evilem-sentence-nav-forward
    "M-S" #'evilem-sentence-nav-backward
    "M-p" #'evilem-forward-paragraph
    "M-P" #'evilem-backward-paragraph)

  :config
  (evilem-make-motion +sneak-forward-f #'evil-snipe-repeat
                      :pre-hook (save-excursion
                                  ;; TODO :bind doesn't work anymore?
                                  (let (evil-snipe-enable-highlight
                                        evil-snipe-enable-incremental-highlight)
                                    (call-interactively #'+snipe-2-f))))
  (evilem-make-motion +sneak-backward-F #'evil-snipe-repeat
                      :pre-hook (save-excursion
                                  (let (evil-snipe-enable-highlight
                                        evil-snipe-enable-incremental-highlight)
                                    (call-interactively #'+snipe-2-F))))
  (evilem-make-motion +sneak-forward-exclusive-t #'evil-snipe-repeat
                      :pre-hook (save-excursion
                                  (let (evil-snipe-enable-highlight
                                        evil-snipe-enable-incremental-highlight)
                                    (call-interactively #'+snipe-2-t))))
  (evilem-make-motion +sneak-backward-exclusive-T #'evil-snipe-repeat
                      :pre-hook (save-excursion
                                  (let (evil-snipe-enable-highlight
                                        evil-snipe-enable-incremental-highlight)
                                    (call-interactively #'+snipe-2-T))))

  (defun +next-line ()
    (interactive)
    (evil-next-line)
    (beginning-of-line))

  (defun +prev-line ()
    (interactive)
    (evil-previous-line)
    (beginning-of-line))

  (evilem-make-motion evilem-next-line
                      #'+next-line
                      :pre-hook (setq evil-this-type 'line)
                      :bind ((temporary-goal-column (current-column))
                             (line-move-visual t)))

  (evilem-make-motion evilem-prev-line
                      #'+prev-line
                      :pre-hook (setq evil-this-type 'line)
                      :bind ((temporary-goal-column (current-column))
                             (line-move-visual t)))

  (evilem-make-motion evilem-forward-word-begin
                      #'evil-forward-word-begin)
  (evilem-make-motion evilem-forward-WORD-begin
                      #'evil-forward-WORD-begin)
  (evilem-make-motion evilem-backward-word-begin
                      #'evil-backward-word-begin)
  (evilem-make-motion evilem-backward-WORD-begin
                      #'evil-backward-WORD-begin)

  (evilem-make-motion evilem-sentence-nav-forward
                      #'sentence-nav-evil-forward)
  (evilem-make-motion evilem-sentence-nav-backward
                      #'sentence-nav-evil-backward)
  (evilem-make-motion +sentence-easymotion
                      (list #'sentence-nav-forward
                            #'sentence-nav-backward))


  (evilem-make-motion evilem-forward-paragraph
                      #'evil-forward-paragraph)
  (evilem-make-motion evilem-backward-paragraph
                      #'evil-backward-paragraph))

(use-package evil-embrace
  :after evil
  :hook
  (after-init . evil-embrace-enable-evil-surround-integration))

(use-package evil-escape
  :after evil
  :hook
  (after-init . evil-escape-mode))

(use-package evil-exchange
  :general (noct-c-map "x" #'evil-exchange))

(use-package evil-indent-plus
  :after init
  :hook
  (after-init . evil-indent-plus-default-bindings))

(use-package evil-lion
  :general
  ('normal
   "gl" #'evil-lion-left
   "gL" #'evil-lion-right))

(general-def 'visual "ta" #'align)

(use-package evil-matchit
  :general
  (general-r "o" #'evilmi-jump-items))

(use-package evil-nerd-commenter
  :general
  (general-t 'normal 'override "c" #'evilnc-comment-or-uncomment-lines)
  ('normal "gc" #'evilnc-comment-operator)
  ('visual "/" #'evilnc-comment-operator))

(use-package evil-numbers
  :general
  ('normal
    "g C-a" #'evil-numbers/inc-at-pt
    "g C-x" #'evil-numbers/dec-at-pt
    "g C-A" #'evil-numbers/inc-at-pt-incremental
    "g C-X" #'evil-numbers/dec-at-pt-incremental))

(use-package evil-snipe
  :commands (+snipe-2-f +snipe-2-F +snipe-2-t +snipe-2-T)
  :general
  ;; if need in normal state instead of 2-letter (usually don't)
  (general-r
    "f" #'evil-snipe-f
    "F" #'evil-snipe-F)
  (general-rr
    "t" #'evil-snipe-t
    "T" #'evil-snipe-T)
  (general-def '(operator visual)
    "t" #'evil-snipe-t
    "T" #'evil-snipe-T
    "z" #'evil-snipe-z
    "Z" #'evil-snipe-Z)
  (general-def 'operator
    "x" #'evil-snipe-x
    "X" #'evil-snipe-X)
  (general-r
    ";" #'evil-snipe-repeat
    "," #'evil-snipe-repeat-reverse)
  :preface
  (defun +evil-snipe--cleanup ()
    "Disables overlays and cleans up after evil-snipe."
    (remove-overlays nil nil 'category 'evil-snipe)
    (remove-hook 'pre-command-hook #'evil-snipe--cleanup))
  (advice-add 'evil-snipe--cleanup :override #'+evil-snipe--cleanup)
  :init
  (+set-defaults! evil-snipe-smart-case t
                  ;; search visible part of buffer
                  evil-snipe-scope 'visible
                  ;; include matches behind cursor for repeating
                  evil-snipe-repeat-scope 'whole-visible
                  ;; tab to refine search (add another char)
                  evil-snipe-tab-increment t
                  ;; don't automatically bind s and S
                  evil-snipe-auto-disable-substitute nil
                  ;; don't automatically bind ; and ,
                  evil-snipe-override-evil-repeat-keys nil
                  ;; the incremental highlight is good enough
                  evil-snipe-enable-highlight nil)
  :config
  ;; define evil-snipe-(z|Z)
  (evil-snipe-def 2 'inclusive "z" "Z")

  (evil-snipe-def 2 'inclusive "f" "F"
                  :forward-fn +snipe-2-f
                  :backward-fn +snipe-2-F)

  (evil-snipe-def 2 'exclusive "t" "T"
                  :forward-fn +snipe-2-t
                  :backward-fn +snipe-2-T))

(use-package evil-surround
  :after evil
  :hook
  (after-init . global-evil-surround-mode)
  :general
  (general-def 'visual evil-surround-mode-map
    "s" #'evil-surr))

(defmacro +ilambda! (functionname &rest args)
  "Create an interactive lambda of existing function `FUNCTIONNAME' with `ARGS'."
  `(lambda () (interactive) (apply #',functionname ',args)))

(use-package evil-textobj-tree-sitter
  :after evil
  :demand t
  :config
  (define-key evil-outer-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "import"
                                                '((python-mode . [(import_statement) @import])
                                                  (rust-mode . [(use_declaration) @import]))))
  (define-key evil-outer-text-objects-map "f" (cons "evil-outer-function" (evil-textobj-tree-sitter-get-textobj "function.outer")))
  (define-key evil-inner-text-objects-map "f" (cons "evil-inner-function" (evil-textobj-tree-sitter-get-textobj "function.inner")))
  (define-key evil-outer-text-objects-map "c" (cons "evil-outer-class" (evil-textobj-tree-sitter-get-textobj "class.outer")))
  (define-key evil-inner-text-objects-map "c" (cons "evil-inner-class" (evil-textobj-tree-sitter-get-textobj "class.inner")))
  (define-key evil-outer-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-inner-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-inner-text-objects-map "a" (cons "evil-inner-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))
  (define-key evil-outer-text-objects-map "a" (cons "evil-outer-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))

  (define-key evil-normal-state-map (kbd "]a") (cons "goto-parameter-start" (+ilambda! evil-textobj-tree-sitter-goto-textobj "parameter.inner")))
  (define-key evil-normal-state-map (kbd "[a") (cons "goto-parameter-start" (+ilambda! evil-textobj-tree-sitter-goto-textobj "parameter.inner" t)))
  (define-key evil-normal-state-map (kbd "]A") (cons "goto-parameter-end" (+ilambda! evil-textobj-tree-sitter-goto-textobj "parameter.inner" nil t)))
  (define-key evil-normal-state-map (kbd "[A") (cons "goto-parameter-end" (+ilambda! evil-textobj-tree-sitter-goto-textobj "parameter.inner" t t)))
  (define-key evil-normal-state-map (kbd "]c") (cons "goto-class-start" (+ilambda! evil-textobj-tree-sitter-goto-textobj "class.outer")))
  (define-key evil-normal-state-map (kbd "[c") (cons "goto-class-start" (+ilambda! evil-textobj-tree-sitter-goto-textobj "class.outer" t)))
  (define-key evil-normal-state-map (kbd "]C") (cons "goto-class-end" (+ilambda! evil-textobj-tree-sitter-goto-textobj "class.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[C") (cons "goto-class-end" (+ilambda! evil-textobj-tree-sitter-goto-textobj "class.outer" t t)))
  (define-key evil-normal-state-map (kbd "]n") (cons "goto-comment-start" (+ilambda! evil-textobj-tree-sitter-goto-textobj "comment.outer")))
  (define-key evil-normal-state-map (kbd "[n") (cons "goto-comment-start" (+ilambda! evil-textobj-tree-sitter-goto-textobj "comment.outer" t)))
  (define-key evil-normal-state-map (kbd "]N") (cons "goto-comment-end" (+ilambda! evil-textobj-tree-sitter-goto-textobj "comment.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[N") (cons "goto-comment-end" (+ilambda! evil-textobj-tree-sitter-goto-textobj "comment.outer" t t)))
  (define-key evil-normal-state-map (kbd "]f") (cons "goto-function-start" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer") (reposition-window)))))
  (define-key evil-normal-state-map (kbd "[f") (cons "goto-function-start" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" t) (reposition-window)))))
  (define-key evil-normal-state-map (kbd "]F") (cons "goto-function-end" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t) (reposition-window)))))
  (define-key evil-normal-state-map (kbd "[F") (cons "goto-function-end" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" t t) (reposition-window))))))

(use-package evil-traces
  :after evil
  :init (defvaralias 'evil-ex-cmd evil--ex-cmd)
  :hook
  (after-init . (lambda ()
  (evil-traces-mode)
  (evil-traces-use-diff-faces))))

(use-package evil-visualstar
  :after evil
  :hook
  (after-init . global-evil-visualstar-mode))

(use-package exato)

(use-package evil-quick-diff
  :after evil
  :hook
  (after-init . evil-quick-diff-install))

(use-package evil-goggles
  :after evil
  :hook
  (after-init . evil-goggles-mode))

(use-package evil-visual-mark-mode
  :after evil
  :hook
  (after-init . evil-visual-mark-mode))

(use-package crux
  :general
  ('insert
    [(shift return)] #'crux-smart-open-line
    [(control shift return)] #'crux-smart-open-line-above))

(use-package avy
  :config
  (general-setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o ?w ?f ?p ?l ?u ?y)
                avy-all-windows nil
                ;; decrease timeout
                avy-timeout-seconds 0.2)

  (general-def '(normal insert)
    "C-." #'avy-resume)

  (general-r "." #'avy-resume)

  (general-after 'evil
    (general-add-advice 'avy-resume :after #'evil-normal-state)))

(use-package repeat-help
  :hook
  ((after-init . repeat-mode)
   (repeat-mode . repeat-help-mode)
   (repeat-help-mode-hook . (lambda ()
                              ;; Restore non-intrusive help as well as verbose prompt
                              (setq repeat-echo-function repeat-help--echo-function)))))

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Backups and auto-save
(+set-defaults! backup-by-copying t
                version-control t
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
(use-package undo-fu-session
  :ghook ('after-init-hook #'undo-fu-session-global-mode))

(use-package vundo
  :general
  (general-t
    "u" #'vundo)
  :config
  (general-setq vundo-glyph-alist vundo-unicode-symbols
                vundo--window-max-height 10))

(+set-defaults!
 ;; based on undo-tree's limits
 undo-limit (* 80 1024 1024)
 undo-strong-limit (* 120 1024 1024)
 undo-strong-limit (* 360 1024 1024))

;;;; Templating
(use-package tempel
  :init
  (defun +tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook ((prog-mode . +tempel-setup-capf)
         (text-mode . +tempel-setup-capf)))

;; Requires no-littering
(unless (bound-and-true-p byte-compile-current-file)
  (require 'no-littering)
  (+set-defaults! tempel-path (list (no-littering-expand-var-file-name "templates")
                                    (expand-file-name "templates/*.eld" +emacs-config-root))
                  auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
                  backup-directory-alist
                  `(("\\`/tmp/" . nil)
                    ("\\`/dev/shm/" . nil)
                    ("." . ,(no-littering-expand-var-file-name "backup/")))))

(provide '+editing)
;;; +editing.el ends here
