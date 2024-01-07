;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'vl-setup))
(setup (:package vl-setup))

(setup (:package no-littering)
  (:require no-littering)
  (:option custom-file (expand-file-name "etc/settings.el" user-emacs-directory)
           auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
           backup-directory-alist `(("\\`/tmp/" . nil)
                                    ("\\`/dev/shm/" . nil)
                                    ("." . ,(no-littering-expand-var-file-name "backup/")))))

(setup files
  (:option backup-by-copying t
           delete-old-versions t
           version-control t
           kept-new-versions 6
           kept-old-versions 3
           require-final-newline t))

(setup (:package gcmh)
  (:hook-into after-init-hook)
  (:option gcmh-verbose init-file-debug
           gcmh-high-cons-threshold (* 128 1024 1024)))

(setup autorevert
  (:option auto-revert-avoid-polling t
           global-auto-revert-non-file-buffers t
           auto-revert-verbose nil)
  (:with-mode global-auto-revert-mode
    (:hook-into after-init-hook)))

(setup recentf
  (:option rencentf-max-saved-items 1000)
  (:hook-into on-first-file-hook))

(setup savehist
  (:hook-into after-init-hook))

(setup delsel
  (:with-mode delete-selection-mode
    (:hook-into after-init-hook)))

(setup (:package modus-themes)
  (:require modus-themes)
  (:option modus-themes-italic-constructs t
           modus-themes-bold-constructs t
           modus-themes-prompts '(background)
           modus-themes-mixed-fonts nil
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
             (rx-backslash blue-cooler)))

  (load-theme 'modus-operandi-tinted :no-confirm))

(setup (:package ef-themes)
  (:option ef-themes-variable-pitch-ui t
           ef-themes-mixed-fonts nil ;t
           ef-themes-headings ; read the manual's entry of the doc string
           '((0 . (variable-pitch light 1.9))
             (1 . (variable-pitch light 1.8))
             (2 . (variable-pitch regular 1.7))
             (3 . (variable-pitch regular 1.6))
             (4 . (variable-pitch regular 1.5))
             (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
             (6 . (variable-pitch 1.3))
             (7 . (variable-pitch 1.2))
             (agenda-date . (semilight 1.5))
             (agenda-structure . (variable-pitch light 1.9))
             (t . (variable-pitch 1.1)))))

(setup (:package stimmung-themes)
  (let* ((theme-path (locate-library "stimmung-themes"))
         (package-path (file-name-directory theme-path)))
    (add-to-list 'custom-theme-load-path package-path)))

(setup (:package breadcrumb)
  (add-hook 'after-init-hook (lambda()
                               (require 'breadcrumb)
                               (setq-default header-line-format
                                             '("  "
                                               (:eval (breadcrumb-project-crumbs)))))))

(setup (:package general))

(eval-when-compile
  (general-create-definer general-leader
    :states '(insert emacs visual normal)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"))

(setup whitespace
  (:option whitespace-style
           '(face trailing missing-newline-at-eof tab-mark))
  (:hook-into prog-mode))

;; Use font from Gsettings from /org/gnome/desktop/interface/
;; The keys read are:
;;  - ‘font-name’
;;  - 'monospace-font-name’
(setq font-use-system-font t)

;; https://github.com/minad/vertico#configuration
(setup emacs
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (setq use-short-answers t))

(setup mouse
  (:option
   ;; paste at keyboard cursor instead of mouse pointer location
   mouse-yank-at-point t))

(setup indent
  (:option indent-tabs-mode nil))

(setup (:package on)
  (:require on))

(setup (:package undo-fu)
  (:option undo-limit (* 400 1024)
           undo-tree-strong-limit (* 3 1024 1024)
           undo-outer-limit (* 48 1024 1024)))

(setup (:package undo-fu-session)
  (:nixpkgs zstd)
  (:with-mode global-undo-fu-session-mode
    (:hook-into undo-fu-mode))
  (:option undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")
           undo-fu-session-compression 'zst))

(setup (:package vundo)
  (:when-loaded
    (:option vundo-glyph-alist vundo-unicode-symbols
             vundo-compact-display t)
    (set-face-attribute 'vundo-default nil :family "Fira Mono")))

(setup (:package which-key)
  (:hook-into on-first-input-hook))

(setup (:package evil evil-collection)
  (:hook-into after-init-hook)
  (:option
   ;; Will be handled by evil-collections
   evil-want-keybinding nil
   evil-want-C-w-delete t
   ;; Make `Y` behave like `D`
   evil-want-Y-yank-to-eol t
   ;; Do not extend visual selection to whole lines for ex commands
   evil-ex-visual-char-range t
   ;; `*` and `#` selects symbols instead of words
   evil-symbol-word-search t
   ;; Only highlight in the current window
   evil-ex-interactive-search-highlight 'selected-window
   ;; Use vim-emulated search implementation
   evil-search-module 'evil-search
   ;; Do not spam with error messages
   evil-kbd-macro-suppress-motion-error t
   evil-undo-system 'undo-fu
   evil-want-fine-undo t
   evil-visual-state-cursor 'hollow
   evil-visual-update-x-selection-p nil
   evil-move-cursor-back nil
   evil-move-beyond-eol t)
  (:when-loaded
    (:also-load evil-collection)
    ;;; delay loading evil-collection modules until they are needed
    (dolist (mode evil-collection-mode-list)
      (dolist (req (or (cdr-safe mode) (list mode)))
        (with-eval-after-load req
          (message "Loading evil-collection for mode %s" req)
          (evil-collection-init (list mode)))))

    (evil-collection-init
     '(help
       (buff-menu "buff-menu")
       calc
       image
       elisp-mode
       replace
       (indent "indent")
       (process-menu simple)
       shortdoc
       tabulated-list
       tab-bar))))


(setup (:package vl-modeline)
  (:hook-into after-init-hook))

(setup (:package vertico vertico-posframe)
  (:with-mode (vertico-mode vertico-posframe-mode vertico-multiform-mode)
    (:hook-into on-first-input-hook))
  (:option vertico-resize nil
           vertico-posframe-poshandler #'posframe-poshandler-frame-top-center
           vertico-multiform-commands
           '((consult-line
              posframe
              (vertico-count . 20)
              (t
               posframe))))
  (defun vl/no-hscroll-in-minibuffer ()
    (setq-local auto-hscroll-mode nil))
  (:with-function vl/no-hscroll-in-minibuffer
    (:hook-into minibuffer-setup-hook)))


(setup (:package spacious-padding)
  (:hook-into after-init-hook))

(setup (:package marginalia)
  (:hook-into on-first-input-hook))

(setup (:package orderless)
  (:option completion-styles '(orderless basic))
  ;; basic completion style needs to be tried first (not as a
  ;; fallback) for TRAMP hostname completion to work. In
  ;; addition, the partial-completion style allows you to use
  ;; wildcards for file completion and partial paths, e.g.,
  ;; /u/s/l for /usr/share/local
  completion-category-overrides '((file (styles basic partial-completion))))


(setup (:package consult consult-dir consult-project-extra)
  (:option xref-show-xrefs-function #'consult-xref
           xref-show-definitions-function #'consult-xref)
  (general-leader
    "f" #'consult-buffer
    "s" #'consult-imenu
    "d" #'consult-flymake
    "a" #'eglot-code-actions
    "/" #'consult-ripgrep
    "r" #'eglot-rename)
  (:when-loaded
    (require 'consult-project-extra)
    (setq consult-buffer-sources
          (cl-delete-if (lambda (s)
                          (memq s '(consult--source-project-buffer-hidden
                                    consult--source-project-recent-file-hidden)))
                        (append '(consult-project-extra--source-file
                                  consult-project-extra--source-project)
                                consult-buffer-sources)))))

(setup xref
  (:when-loaded
    (:option xref-prompt-for-identifier (append xref-prompt-for-identifier (list #'xref-find-references)))))

(setup (:package embark embark-consult))

(setup (:package corfu nerd-icons-corfu)
  (:with-mode global-corfu-mode
    (:hook-into on-first-input-hook))
  (:when-loaded
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(setup emacs
  (:global [remap kill-buffer] #'kill-current-buffer)
  ;; TAB cycle if there are only few candidates
  (:option completion-cycle-threshold 3
           ;; Enable indentation+completion using the TAB key.
           ;; `completion-at-point' is often bound to M-TAB.
           tab-always-indent t))

(setup (:package envrc)
  (:with-mode envrc-global-mode
    (:hook-into on-first-file-hook))

  (defun vl/direnv-init-global-mode-earlier-h ()
    (let ((fn #'envrc-global-mode-enable-in-buffers))
      (if (not envrc-global-mode)
          (remove-hook 'change-major-mode-after-body-hook fn)
        (remove-hook 'after-change-major-mode-hook fn)
        (add-hook 'change-major-mode-after-body-hook fn 100))))
  (add-hook 'envrc-global-mode-hook #'vl/direnv-init-global-mode-earlier-h)

  (defvar vl/orig-exec-path exec-path)
  (defun vl/envrc--debounce-add-extra-path-a (fn &rest args)
    "Update only on non internal envrc related buffers keeping original path entries as well"
    (when (not (string-prefix-p "*envrc" (buffer-name)))
      (apply fn args)
      (setq-local exec-path (append exec-path vl/orig-exec-path))))

  (advice-add #'envrc--update :around #'vl/envrc--debounce-add-extra-path-a))

;; (setup eglot
;;   (:option eglot-extend-to-xref t)
;;   (:with-mode eglot-inlay-hints-mode
;;     (:hook-into eglot-managed-mode-hook))
;;   (:when-loaded
;;     (:option
;;      (prepend eglot-server-programs)
;;      '(rustic-mode .
;;                    ("rust-analyzer"
;;                     :initializationOptions
;;                     (:checkOnSave (:command "clippy")))))))

;; (setup-define :lsp
;;   (lambda ()
;;     `(:hook eglot-ensure))
;;   :documentation "Configure LSP for given mode.")



(setup (:package lsp-mode)
  (:with-function lsp-enable-which-key-integration
    (:hook-into lsp-mode-hook))
  (:with-function lsp-deferred
    (:hook-into python-base-mode))
  (:option read-process-output-max (* 4 1024 1024)
           lsp-enable-suggest-server-download nil
           lsp-inlay-hint-enable t
           lsp-keep-workspace-alive nil
           lsp-response-timeout 30
           lsp-diagnostics-provider :flycheck
           lsp-headerline-breadcrumb-enable nil
           lsp-semantic-tokens-enable t
           lsp-file-watch-threshold 4000
           lsp-keymap-prefix "C-C l")
  (general-def '(motion normal) lsp-mode-map
    "gd" #'xref-find-definitions
    "gr" #'xref-find-references))

(setup-define :lsp
  (lambda ()
    `(:hook lsp-deferred))
  :documentation "Configure LSP for given mode.")

(defun lsp-booster--json-parse-bufffer-a (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add 'json-parse-buffer :around #'lsp-booster--json-parse-bufffer-a)

(defun lsp-booster--resolve-final-command-a (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command :around #'lsp-booster--resolve-final-command-a)

(setup (:package yasnippet)
  (:with-mode yas-global-mode
    (:hook-into on-first-buffer-hook)))

(setup (:package flycheck))
;; (setup (:package flycheck-popup-tip)
;;   (with-eval-after-load 'evil
;;     (add-hook 'evil-insert-state-entry-hook #'flycheck-popup-tip-delete-popup)
;;     (add-hook 'evil-replace-state-entry-hook #'flycheck-popup-tip-delete-popup)))
;; (setup (:package flycheck-posframe)
;;   (:option flycheck-posframe-warning-prefix "! "
;;         flycheck-posframe-info-prefix "··· "
;;         flycheck-posframe-error-prefix "X ")
;;   (with-eval-after-load 'evil
;;     ;; Don't display popups while in insert or replace mode, as it can affect
;;     ;; the cursor's position or cause disruptive input delays.
;;     (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p
;;               (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p))))

(setup (:package lsp-ui)
  (:option lsp-uis-sideline-show-diagnostics nil))

;; (setup flymake
;;   (:package flymake-popon)
;;   (:hook-into prog-mode-hook)
;;   (:with-mode flymake-popon-mode
;;     (:hook-into flymake-mode-hook))
;;   (:option flymake-popon-method 'posframe))

;; (setup (:package lsp-bridge)
;;   (:with-mode global-lsp-bridge-mode
;;     (:hook-into after-init-hook))
;;   (:option lsp-bridge-enable-hover-diagnostic t)
;;   (general-def 'motion lsp-bridge-mode-map
;;     "gd" #'lsp-bridge-find-def
;;     "gr" #'lsp-bridge-find-references))

(setup (:package treesit-auto)
  (:require treesit-auto)
  (:with-mode global-treesit-auto-mode
    (:hook-into after-init-hook)))

(setup elisp-mode (:package highlight-quoted rainbow-delimiters)
       (:with-mode (outline-minor-mode rainbow-delimiters-mode highlight-quoted-mode)
         (:hook-into emacs-lisp-mode-hook)))

(setup (:package jinx)
  (:with-mode global-jinx-mode
    (:hook-into on-first-buffer-hook))
  (:option jinx-languages "en_US hu_HU"))

(setup (:package apheleia)
  (:with-mode apheleia-global-mode
    (:hook-into on-first-buffer-hook)))

(setup (:package ws-butler)
  (:hook-into prog-mode-hook))

(setup (:package diff-hl)
  (:with-mode global-diff-hl-mode
    (:hook-into on-first-buffer-hook))
  (:with-mode diff-hl-dired-mode
    (:hook-into dired-mode-hook)))

(setup (:package magit)
  (:option magit-prefer-remote-upstream t
           magit-save-repository-buffers nil
           magit-diff-refine-hunk t
           magit-diff-refine-hunk t
           magit-bury-buffer-function #'magit-restore-window-configuration
           magit-display-buffer-function #'magit-display-buffer-fullframe-status-topleft-v1))


(setup (:package nix-mode)
  (:lsp)
  (:nixpkgs ("nil" nixpkgs-fmt))
  (:smartparens
   (sp-local-pair
    "let" "in"
    :post-handlers '(("||\n[i]" "RET"))
    :unless '(sp-point-before-word-p sp-point-before-same-p))

   (sp-local-pair
    "''" "''"
    :post-handlers '(("||\n[i]" "RET"))
    :unless '(sp-point-before-word-p sp-point-before-same-p))

   (sp-local-pair
    "'" nil
    :actions nil)

   (sp-local-pair
    "${" "}"
    :unless '(sp-point-before-word-p sp-point-before-same-p))))

(setup python-base-mode
  (:package lsp-pyright)
  (:when-loaded
    (:require lsp-pyright))
  (:lsp)
  (:nixpkgs pyright))
(setup (:package yaml-mode))

(setup (:package rustic)
  (:lsp)
  (:nixpkgs rust-analyzer)
  (:option rustic-lsp-client nil)
  (:file-match "\\.rs$"))

(setup (:package tuareg dune utop))

(setup (:package fsharp-mode)
  (:lsp)
  (:nixpkgs fsautocomplete))

(setup (:package explain-pause-mode))

(setup (:package smartparens)
  (:option sp-navigate-skip-match nil
           sp-navigate-consider-sgml-tags nil)
  (:with-mode smartparens-global-mode
    (:hook-into on-first-buffer-hook))
  (:when-loaded
    (require 'smartparens-config)

    (let ((unless-list '(sp-point-before-word-p
                         sp-point-after-word-p
                         sp-point-before-same-p)))
      (sp-pair "'"  nil :unless unless-list)
      (sp-pair "\"" nil :unless unless-list))

    ;; Expand {|} => { | }
    ;; Expand {|} => {
    ;;   |
    ;; }
    (dolist (brace '("(" "{" "["))
      (sp-pair brace nil
               :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
               ;; Don't autopair opening braces if before a word character or
               ;; other opening brace. The rationale: it interferes with manual
               ;; balancing of braces, and is odd form to have s-exps with no
               ;; whitespace in between, e.g. ()()(). Insert whitespace if
               ;; genuinely want to start a new form in the middle of a word.
               :unless '(sp-point-before-word-p sp-point-before-same-p)))

    ;; In lisps ( should open a new form if before another parenthesis
    (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

    ;; Don't do square-bracket space-expansion where it doesn't make sense to
    (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                   "[" nil :post-handlers '(:rem ("| " "SPC")))

    ;; Reasonable default pairs for HTML-style comments
    (sp-local-pair (append sp--html-modes '(markdown-mode gfm-mode))
                   "<!--" "-->"
                   :unless '(sp-point-before-word-p sp-point-before-same-p)
                   :actions '(insert) :post-handlers '(("| " "SPC")))))

(setup dockerfile-ts-mode
  (:file-match (concat "[/\\]"
                       "\\(?:Containerfile\\|Dockerfile\\)"
                       "\\(?:\\.[^/\\]*\\)?\\'")))

(setup (:package julia-mode eglot-jl)
  (:lsp)
  (:when-loaded
    (:require eglot-jl)
    (eglot-jl-init)))

(setup (:package julia-repl)
  (:hook-into julia-mode-hook))

(setup (:package bufferlo)
  (:hook-into on-first-input-hook)
  (with-eval-after-load 'consult
    (require 'bufferlo)
    (defvar my-consult--source-buffer
      `(:name "All Buffers"
              :narrow   ?a
              :hidden   t
              :category buffer
              :face     consult-buffer
              :history  buffer-name-history
              :state    ,#'consult--buffer-state
              :items ,(lambda () (consult--buffer-query
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "All buffer candidate source for `consult-buffer'.")

    (defvar my-consult--source-local-buffer
      `(:name "Local buffers"
              :narrow   ?b
              :category buffer
              :face     consult-buffer
              :history  buffer-name-history
              :state    ,#'consult--buffer-state
              :default  t
              :items ,(lambda () (consult--buffer-query
                                  :predicate #'bufferlo-local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Local buffer candidate source for `consult-buffer'.")
    (add-to-list 'consult-buffer-sources 'my-consult--source-buffer)
    (add-to-list 'consult-buffer-sources 'my-consult--source-local-buffer)

    (consult-customize consult--source-buffer :hidden t :default nil)))

