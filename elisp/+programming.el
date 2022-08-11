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
(require '+packages)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'flymake-mode)

(+install! tree-sitter)
(+install! tree-sitter-langs)

(defun +setup-tree-sitter-mode ()
  (global-tree-sitter-mode)
  (require 'tree-sitter-langs))

(add-hook 'after-init-hook #'+setup-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(+install! eglot)

(defvar +eglot-pyright-executable "pyright-langserver")
(defvar +eglot-rust-analyzer-executable "rust-analyzer")

(+after! eglot
  (add-to-list 'eglot-server-programs `(python-mode . (,+eglot-pyright-executable "--stdio")))
  (add-to-list 'eglot-server-programs `(rust-mode . ,+eglot-rust-analyzer-executable)))

(+install! nix-mode)
(add-hook 'nix-mode-hook #'eglot-ensure)

(add-hook 'python-mode-hook #'eglot-ensure)

(+install! pyvenv)

(add-hook 'python-mode-hook #'pyvenv-tracking-mode)
(set-defaults '(pyvenv-default-virtual-env-name ".venv"))

(+install! rust-mode)
(+install! rustic)

(defun +setup-rustic-mode ()
  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)) 0 'local))

(add-hook 'rustic-mode-hook #'+setup-rustic-mode)
(set-defaults
 '(rustic-lsp-client 'eglot)
 '(rust-indent-method-chain t))

(+install! flycheck)
(+after! rustic-flycheck
  (add-to-list 'flycheck-checkers 'rustic-clippy))

(+install! dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(defun +dumb-jump-nix-derivation-root (filepath)
  "Predicate returning when we are at a nix derivation's root directory"
  (when (string-match-p "^/nix/store/[^/]+$" "/nix/store/foo")
    filepath))

;; When dumb-jump couldn't find a project directory, check if we are in a nix derivation,
(advice-add #'dumb-jump-get-config :after-until #'+dumb-jump-nix-derivation-root)

(provide '+programming)

;;; +programming.el ends here
