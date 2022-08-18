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

(+install! flymake)
(defun +setup-prog-mode ()
  (display-line-numbers-mode)
  (flymake-mode)
  (electric-pair-mode))

(add-hook 'prog-mode-hook #'+setup-prog-mode)

(+set-defaults!
 display-line-numbers-width 3
 display-line-numbers-widen t
 comment-empty-lines t
 electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit
 eldoc-echo-area-display-truncation-message nil
 eldoc-echo-area-use-multiline-p 3
 eldoc-echo-area-prefer-doc-buffer t)

(+define-keys! flymake
  (flymake-mode-map
   (((kbd "C-c ! n") #'flymake-goto-next-error)
    ((kbd "C-c ! p") #'flymake-goto-prev-error))))

(+install! tree-sitter)
(+install! tree-sitter-langs)

(defun +setup-tree-sitter-mode ()
  (global-tree-sitter-mode)
  (require 'tree-sitter-langs))

(add-hook 'after-init-hook #'+setup-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;;;; LSP

(+install! eglot)
(+install! eglot-x)
(+install! consult-eglot)

(+after! eglot
  (require 'eglot-x))

(+define-keys! eglot
  (eglot-mode-map
   (((kbd "C-c C-l r") #'eglot-rename)
    ((kbd "C-c C-l a") #'eglot-code-actions)
    ((kbd "C-c C-l f") #'eglot-format)
    ((kbd "C-c C-l C-f d") #'eglot-find-declaration)
    ((kbd "C-c C-l C-f i") #'eglot-find-implementation)
    ((kbd "C-c C-l C-f t") #'eglot-find-typeDefinition))))
(+define-key! consult-eglot eglot-mode-map (kbd "M-g s") #'consult-eglot-symbols)


;;;; Python
(+install! pyvenv)
(+install! python-docstring)
(+install! poetry)
(+install! pippel)
(+install! python-pytest)
(+install! pdb-capf)

(+set-defaults!
 pyvenv-default-virtual-env-name ".venv"
 python-pytest-executable "python -m pytest")

(+define-key! python-mode python-mode-map (kbd "C-c t")  #'python-pytest-dispatch)

(add-hook 'python-mode-hook #'+setup-python-mode)
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

(+install! realgud)

(+install! direnv)
(+install! docker)
(+install! docker-tramp)
(+install! dockerfile-mode)
(+install! gitlab-ci-mode)
(+install! graphql-mode)
(+install! json-mode)
(+install! just-mode)
(+install! lua-mode)
(+install! nix-mode)
(+install! rust-mode)
(+install! rustic)
(+install! typescript-mode)
(+install! web-mode)
(+install! yaml-mode)
(+install! zig-mode)

(+set-defaults!
 eglot-autoshutdown t
 direnv-always-show-summary nil
 pyvenv-default-virtual-env-name ".venv"
 rust-indent-method-chain t
 rustic-lsp-client 'eglot)

(add-hook 'after-init-hook #'direnv-mode)
(add-hook 'c++-mode-hook #'eglot-ensure)
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'css-mode-hook #'eglot-ensure)
(add-hook 'dockerfile-mode-hook #'eglot-ensure)
(add-hook 'js-mode-hook #'eglot-ensure)
(add-hook 'lua-mode-hook #'eglot-ensure)
(add-hook 'nix-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'sh-mode-hook #'eglot-ensure)
(add-hook 'typescript-mode-hook #'eglot-ensure)
(add-hook 'zig-mode-hook #'eglot-ensure)

(defun +flymake-json-mode-setup ()
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-jsonlint nil t))

(add-hook 'json-mode-hook #'+flymake-json-mode-setup)

(defun +flymake-sh-mode-setup ()
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-shellcheck nil t))

(add-hook 'sh-mode-hook #'+flymake-sh-mode-setup)

(defun +flymake-yaml-mode-setup ()
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-yamllint nil t))

(add-hook 'yaml-mode-hook #'+flymake-yaml-mode-setup)

(+install! dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(defun +dumb-jump-nix-derivation-root (filepath)
  "Predicate returning when FILEPATH is inside a nix derivation's root directory."
  (when (string-match-p "^/nix/store/[^/]+$" "/nix/store/foo")
    filepath))

;; When dumb-jump couldn't find a project directory, check if we are in a nix derivation,
(advice-add #'dumb-jump-get-config :after-until #'+dumb-jump-nix-derivation-root)


;;;; Apheleia
(+install! apheleia)

(autoload #'apheleia-global-mode "apheleia" nil t)
(add-hook 'after-init-hook #'apheleia-global-mode)

;; resolve formatter commands through `executable-find' to be able to
;; find commands coming through `'nix-integration' or `direnv' and
;; similar tools
(defun +advised-apheleia--run-formatter-process (fn command &rest args)
  (let ((exe (executable-find (car command)))
        (argv (cdr command)))
    (apply fn `((,exe ,@argv) ,@args))))

(advice-add 'apheleia--run-formatter-process :around #'+advised-apheleia--run-formatter-process)

(+after! apheleia
  (setf (alist-get 'nixpkgs-fmt apheleia-formatters) (list "nixpkgs-fmt"))
  (setf (alist-get 'nix-mode apheleia-mode-alist) 'nixpkgs-fmt)
  (setf (alist-get 'python-mode apheleia-mode-alist) '(isort black)))

(provide '+programming)

;;; +programming.el ends here
