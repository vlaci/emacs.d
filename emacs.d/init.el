(eval-when-compile
  (require 'vl-setup))

(setup (:package vlaci))

(setup (:package no-littering)
  (:set custom-file (expand-file-name "etc/settings.el" user-emacs-directory))
  (:require no-littering))

(setup (:package gcmh)
  (:hook-into after-init-hook)
  (:set gcmh-verbose init-file-debug
        gcmh-high-cons-threshold (* 128 1024 1024)))

(setup autorevert
  (:set auto-revert-avoid-polling t)
  (:with-mode global-auto-revert-mode
    (:hook-into after-init-hook)))


(setup delsel
  (:with-mode delete-selection-mode
    (:hook-into after-init-hook)))


(setup (:package modus-themes)
  (:require modus-themes)
  (:set modus-themes-italic-constructs t
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
           (rx-backslash blue-cooler)))

  (load-theme 'modus-operandi-tinted :no-confirm))

(setup (:package mini-echo)
  (:hook-into after-init-hook)
  (custom-set-faces
   `(mini-echo-minibuffer-window ((t :overline 'gray)))))

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
  (setq enable-recursive-minibuffers t))

(setup (:package vertico)
  (:hook-into after-init-hook))

(setup (:package vertico-posframe)
  (:hook-into vertico-mode))
