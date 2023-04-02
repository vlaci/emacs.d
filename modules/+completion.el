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
(require '+lib)

(use-package recursion-indicator
  :after vertico
  :config
  (recursion-indicator-mode))

(use-package vertico
  :hook (after-init . vertico-mode)
  :init
  (+set-defaults!
   vertico-cycle t
   vertico-count 20
   vertico-multiform-categories '((consult-grep buffer))
   vertico-multiform-commands '((consult-imenu buffer indexed)
                                (consult-imenu-multi buffer indexed))
   vertico-buffer-display-action '((display-buffer-in-side-window)
                                   (side . right)
                                   (slot . 0)
                                   (window-width . 100)))
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (minibuffer-electric-default-mode)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (vertico-multiform-mode)
  (setq enable-recursive-minibuffers t)
  (setq-default
   mode-line-modes
   (seq-filter (lambda (s)
                 (not (and (stringp s)
                           (string-match-p
                            "^\\(%\\[\\|%\\]\\)$" s))))
               mode-line-modes)))

(defun +crm-indicator (args)
  (defvar crm-separator)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'+crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


(use-package marginalia
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map ("M-a" . marginalia-cycle)))

(use-package consult
  :init
  (+set-defaults!
   meow-goto-line-function #'consult-goto-line)

  (general-define-key
   :keymaps 'mode-specific-map
   "h" #'consult-history
   "m" #'consult-mode-command
   "C-k" #'consult-kmacro)

  (general-define-key
   :keymaps 'ctl-x-map
   "M-:" #'consult-complex-command     ;; orig. repeat-complex-command
   "b" #'consult-buffer                ;; orig. switch-to-buffer
   "C-b" #'consult-buffer              ;; orig. buffer-menu
   "4 b" #'consult-buffer-other-window ;; orig. switch-to-buffer-other-window
   "5 b" #'consult-buffer-other-frame  ;; orig. switch-to-buffer-other-frame
   "r b" #'consult-bookmark            ;; orig. bookmark-jump
   "p b" #'consult-project-buffer      ;; orig. project-switch-to-buffer
   "C-r" #'consult-recent-file)        ;; orig. find-file-read-only
  (general-define-key
   :keymaps 'global-map
   "M-#" #'consult-register-load
   "M-'" #'consult-register-store      ;; orig. abbrev-prefix-mark (unrelated)
   "C-M-#" #'consult-register
   "M-y" #'consult-yank-pop)           ;; orig. yank-pop
  (general-define-key
   :keymaps 'goto-map ;; M-g
   "e" #'consult-compile-error
   "f" #'consult-flymake               ;; Alternative: consult-flycheck
   "g" #'consult-goto-line             ;; orig. goto-line
   "M-g" #'consult-goto-line           ;; orig. goto-line
   "o" #'consult-outline               ;; Alternative: consult-org-heading
   "m" #'consult-mark
   "k" #'consult-global-mark
   "i" #'consult-imenu
   "I" #'consult-imenu-multi)
  (general-define-key
   :keymaps 'search-map ;; M-s
   "d" #'consult-find
   "D" #'consult-locate
   "g" #'consult-grep
   "G" #'consult-git-grep
   "r" #'consult-ripgrep
   "l" #'consult-line
   "L" #'consult-line-multi
   "m" #'consult-multi-occur
   "k" #'consult-keep-lines
   "u" #'consult-focus-lines
   "e" #'consult-isearch-history)
  (general-define-key
   :keymaps 'isearch-mode-map
   "M-e" #'consult-isearch-history     ;; orig. isearch-edit-string
   "M-s e" #'consult-isearch-history   ;; orig. isearch-edit-string
   "M-s l" #'consult-line              ;; needed by consult-line to detect isearch
   "M-s L" #'consult-line-multi)       ;; needed by consult-line to detect isearch
  (general-define-key
   :keymaps 'minibuffer-local-map
   "M-s" #'consult-history             ;; orig. next-matching-history-element
   "M-r" #'consult-history)            ;; orig. previous-matching-history-element
  (general-define-key
   :keymaps 'consult-narrow-map
   "?" #'consult-narrow-help)

  (defvar +consult-project-history nil)

  (defvar +consult-source-project
    `( :name "Known Project"
       :narrow    (?p . "Project")
       :category  'consult-project-extra-project
       :face      consult-file
       :history   +consult-project-history
       :annotate  ,(lambda (dir) dir)
       :action    ,#'project-switch-project
       :items     ,(lambda ()
                     (let ((projects (project-known-project-roots)))
                       (mapcar (lambda (project)
                                 (let ((dir (file-name-nondirectory (directory-file-name project))))
                                   (put-text-property 0 (length dir) 'multi-category `(file . ,project) dir)
                                   dir))
                               projects)))))

  (defvar +consult-source-project-file
    `( :name "Project File"
       :narrow    (?f . "Project File")
       :category  file
       :face      consult-file
       :history   file-name-history
       :action    ,#'consult--file-action
       :enabled   ,(lambda () consult-project-function)
       :items
       ,(lambda ()
          (when-let* ((root (consult--project-root))
                      (buffers (consult--buffer-file-hash))
                      (project (project--find-in-directory root)))
            (mapcar (lambda (file)
                      (let ((root-part (substring file (length root))))
                        (when (string= root-part "")
                          (setq root-part "./"))
                        (put-text-property 0 (length root-part) 'multi-category `(file . , file) root-part)
                        root-part))
                    (seq-filter (lambda (file) (not (gethash file buffers)))
                                (project-files project)))))))
  :config
  (dolist
      (pattern (list
                "\\`\\*Messages\\*\\'"
                "\\`\\*Async-native-compile-log\\*\\'"
                "\\`magit-"
                "\\`\\*EGLOT"
                "\\`\\*direnv\\*\\'"))
    (add-to-list 'consult-buffer-filter pattern))
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "M-."
   consult-buffer :sort nil
   consult--source-project-buffer :hidden nil
   consult--source-recent-file :narrow '(?r . "Recent File")
   consult--source-project-buffer :narrow '(?B . "Project Buffer")
   consult--source-project-recent-file :narrow '(?R . "Project Recent File"))

  (+set-defaults!
   consult-buffer-sources
   '(consult--source-hidden-buffer
     consult--source-modified-buffer
     consult--source-project-recent-file
     +consult-source-project-file
     consult--source-buffer
     consult--source-recent-file
     +consult-source-project
     consult--source-bookmark))

  (+after! xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)))

(use-package consult-dir
  :after vertico
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


(use-package orderless
  :after (:any vertico consult)
  :demand t
  :init
  (defun +orderless--suffix-regexp ()
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

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
  :config
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
        orderless-style-dispatchers '(+orderless-dispatch)))

(use-package embark-consult)
(use-package embark
  :bind (([remap describe-bindings] . embark-bindings)
         ("C-." . #'embark-act)
         ("C-;" . #'embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  (defmacro +embark-ace-action (fn)
    `(defun ,(intern (concat "+embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn))))))

  (+set-defaults!
   embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :config
  (require 'embark-consult)
  (require 'ace-window)
  (+after! ace-window
    (define-key embark-file-map     (kbd "o") (+embark-ace-action find-file))
    (define-key embark-buffer-map   (kbd "o") (+embark-ace-action switch-to-buffer))
    (define-key embark-bookmark-map (kbd "o") (+embark-ace-action bookmark-jump))))


(add-to-list 'display-buffer-alist
             '("\\`\\*Embark"
               (display-buffer-in-side-window)
               (side . right)
               (slot . -1)
               (window-width . 0.4)))

(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Live"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 30)))

(add-to-list 'display-buffer-alist
             '("\\` \\*Embark Actions\\*"
               (display-buffer-in-side-window)
               (side . bottom)
               (slot . 1)
               (window-height . fit-window-to-buffer)))

(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(defun +embark-live-vertico ()
  "Shrink Vertico minibuffer when `embark-live' is active."
  (when-let (win (and (string-prefix-p "*Embark Live" (buffer-name))
                      (active-minibuffer-window)))
    (with-selected-window win
      (when (and (bound-and-true-p vertico--input)
                 (fboundp 'vertico-multiform-unobtrusive))
        (vertico-multiform-unobtrusive)))))

(add-hook 'embark-collect-mode-hook #'+embark-live-vertico)

;;(autoload 'corfu-insert-separator "corfu")
(use-package corfu
  :init
  (+set-defaults!
   tab-always-indent 'complete
   tab-fir 'word-or-paran-or-punct
   corfu-cycle t
   corfu-echo-documentation 0.3)

  (defun +setup-corfu-mode ()
    (global-corfu-mode)
    (corfu-history-mode)
    (corfu-popupinfo-mode))
  (defun +corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (bound-and-true-p vertico--input)
    (corfu-mode 1)))

  (defun +corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :hook ((after-init . +setup-corfu-mode)
         (eshell-mode .
          (lambda () (setq-local corfu-quit-at-boundary t
                                 corfu-quit-no-match t)
            (corfu-mode)))
         (minibuffer-setup . +corfu-enable-always-in-minibuffer))
  :bind (:map corfu-map
         ("M-m" . +corfu-move-to-minibuffer)
         ("SPC" . corfu-insert-separator)))

(use-package cape
  :init
  (dolist (backend (list #'cape-symbol #'cape-keyword #'cape-file #'cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend))

  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;;;; Project

(global-set-key [remap switch-to-buffer] #'consult-buffer)
(global-set-key [remap list-buffer] #'consult-buffer)
(global-set-key [remap project-switch-to-buffer] #'consult-project-buffer)
(global-set-key [f1] #'consult-buffer)
(global-set-key [f2] #'consult-ripgrep)

(+after! project
  (define-key project-prefix-map "r" #'consult-ripgrep)
  (add-to-list 'project-switch-commands '(consult-ripgrep "rg") t))

(provide '+completion)
;;; +completion.el ends here
