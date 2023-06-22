;;; +programming.el --- summary -*- lexical-binding: t -*-

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

(defun +setup-prog-mode ()
  (display-line-numbers-mode))

(add-hook 'prog-mode-hook #'+setup-prog-mode)

(+set-defaults!
 display-line-numbers-width 3
 display-line-numbers-widen t
 comment-empty-lines t
 eldoc-echo-area-display-truncation-message nil
 eldoc-echo-area-use-multiline-p 3
 eldoc-echo-area-prefer-doc-buffer t
 elisp-flymake-byte-compile-load-path load-path)

(general-define-key
 :keymaps 'flymake-mode-map
 "C-c ! n" #'flymake-goto-next-error
 "C-c ! p" #'flymake-goto-prev-error)

(use-package treesit-auto
  :demand t
  :config
  (global-treesit-auto-mode))

;;;; LSP

(use-package eglot-x
  :after eglot)

(use-package consult-eglot)

(general-define-key
 :keymaps 'eglot-mode-map
 "C-c C-l r" #'eglot-rename
 "C-c C-l a" #'eglot-code-actions
 "C-c C-l f" #'eglot-format
 "C-c C-l C-f d" #'eglot-find-declaration
 "C-c C-l C-f i" #'eglot-find-implementation
 "C-c C-l C-f t" #'eglot-find-typeDefinition
 "M-g s" #'consult-eglot-symbols)

(defun +advised-eglot--cmd (eglot--cmd-fn contact)
  (let ((cmd (executable-find (car contact)))
        (argv (cdr contact)))
    (funcall eglot--cmd-fn `(,cmd ,@argv))))

(advice-add 'eglot--cmd :around #'+advised-eglot--cmd)

(+set-defaults!
 eglot-extend-to-xref t
 eglot-report-progress t)

;;;; Haskell
(use-package haskell-mode
  :hook (haskell-mode . eglot-ensure))

;;;; Julia
(use-package eglot-jl
  :after eglot
  :config
  (eglot-jl-init))
(use-package julia-mode
  :hook (julia-mode . eglot-ensure))

(use-package julia-repl)

;;;; OCaml
(use-package dune)
(use-package utop)
(use-package tuareg
  :hook (tuareg-mode . eglot-ensure))

;;;; Python
(use-package pyvenv
  :ghook ('python-base-mode-hook #'+pyvenv-autoload))

(cl-defun +pyvenv-autoload ()
  "Automatically activates pyvenv version if .venv directory exists."
  (f-traverse-upwards
   (lambda (path)
     (let ((venv-path (f-expand ".venv" path)))
       (when (f-exists? venv-path)
         (pyvenv-activate venv-path)
         (cl-return-from +pyvenv-autoload))))))

(use-package python-docstring)
(use-package poetry)
(use-package pippel)
(use-package pdb-capf)

(+set-defaults!
 python-indent-def-block-scale 1)

(add-hook 'python-base-mode-hook #'+setup-python-mode)
(add-hook 'pdb-mode-hook #'+setup-pdb-capf)
(add-hook 'pdb-track-mode-hook #'+setup-pdb-capf)
(add-hook 'python-pytest-mode-hook #'+setup-pdb-capf)

(defun +setup-python-mode ()
  "Defaults for Python development."
  (setq-local tab-width 4)
  (pyvenv-tracking-mode)
  (python-docstring-mode))

(defun +setup-pdb-capf ()
  "Tab completion in pdb prompt."
  (add-to-list 'completion-at-point-functions #'pdb-capf))

;;;; Rust
(use-package rust-mode)
(use-package rustic)

(+set-defaults!
 rust-indent-method-chain t
 rustic-lsp-client 'eglot)

(+after! eglot
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (add-to-list 'eglot-server-programs
               '(rustic-mode .
                             ("rust-analyzer"
                              :initializationOptions
                              (:checkOnSave (:command "clippy"))))))

(use-package realgud)

(use-package direnv)
(use-package docker)
(use-package dockerfile-mode)
(use-package gitlab-ci-mode)
(use-package graphql-mode)
(use-package json-mode)
(use-package just-mode)
(use-package lua-mode)
(use-package nix-mode)
(use-package nushell-mode)
(use-package typescript-mode)
(use-package web-mode)
(use-package yaml-mode)
(use-package zig-mode)

(+set-defaults!
 eglot-autoshutdown t
 direnv-always-show-summary nil
 pyvenv-default-virtual-env-name ".venv"
 rust-indent-method-chain t
 rustic-lsp-client 'eglot)

(add-hook 'after-init-hook #'direnv-mode)

(dolist (hook (list
               #'python-ts-mode-hook
               #'python-mode-hook
               #'js-ts-mode-hook
               #'js-mode-hook
               #'json-mode-hook
               #'json-ts-mode-hook
               #'typescript-ts-mode-hook
               #'typescript-mode-hook
               #'tsx-ts-mode-hook
               #'bash-ts-mode-hook
               #'bash-mode-hook
               #'sh-mode-hook
               #'c-ts-mode-hook
               #'c-mode-hook
               #'c++-ts-mode-hook
               #'c++-mode-hook
               #'haskell-mode-hook
               #'caml-mode-hook
               #'tuareg-mode-hook
               #'yaml-ts-mode-hook
               #'yaml-mode-hook
               #'nix-mode-hook
               #'lua-mode-hook
               #'zig-mode-hook
               #'css-ts-mode-hook
               #'css-mode-hook
               #'html-mode-hook
               #'dockerfile-ts-mode-hook
               #'dockerfile-mode-hook
               #'markdown-mode-hook))
  (add-hook hook #'eglot-ensure))

(use-package combobulate)
(autoload #'combobulate-mode "combobulate" nil t)
(dolist  (hook (list
                'python-ts-mode-hook
                'js-ts-mode-hook
                'css-ts-mode-hook
                'yaml-ts-mode-hook
                'typescript-ts-mode-hook
                'tsx-ts-mode-hook))
  (add-hook hook #'combobulate-mode))

(defun +flymake-json-mode-setup ()
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-jsonlint nil t))

(add-hook 'json-mode-hook #'+flymake-json-mode-setup)

(defun +flymake-sh-mode-setup ()
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-shellcheck nil t))

(add-hook 'sh-mode-hook #'+flymake-sh-mode-setup)

(defun +flymake-yaml-mode-setup ()
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-yamllint nil t))

(add-hook 'yaml-mode-hook #'+flymake-yaml-mode-setup)

(use-package dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(defun +dumb-jump-nix-derivation-root (filepath)
  "Predicate returning when FILEPATH is inside a nix derivation's root directory."
  (when (string-match-p "^/nix/store/[^/]+$" "/nix/store/foo")
    filepath))

;; When dumb-jump couldn't find a project directory, check if we are in a nix derivation,
(advice-add #'dumb-jump-get-config :after-until #'+dumb-jump-nix-derivation-root)


;;;; Apheleia
(use-package apheleia
  :hook (after-init . apheleia-global-mode))

;; resolve formatter commands through `executable-find' to be able to
;; find commands coming through `'nix-integration' or `direnv' and
;; similar tools
(defun +advised-apheleia--run-formatter-process (fn command &rest args)
  (let* ((command (if (symbolp (car command)) (cdr command) command))
         (exe (executable-find (car command)))
         (argv (cdr command)))
    (apply fn `((,exe ,@argv) ,@args))))

(advice-add 'apheleia--run-formatter-process :around #'+advised-apheleia--run-formatter-process)

(+after! apheleia
  (setf (alist-get 'nixpkgs-fmt apheleia-formatters) (list "nixpkgs-fmt"))
  (setf (alist-get 'nix-mode apheleia-mode-alist) 'nixpkgs-fmt)
  (setf (alist-get 'python-mode apheleia-mode-alist) '(isort black))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(isort black)))

(use-package cov)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(provide '+programming)

;;; +programming.el ends here
