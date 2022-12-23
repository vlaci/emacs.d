;;; +grammatical-edit.el --- structural editing -*- lexical-binding: t -*-

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

(+install! grammatical-edit)

(dolist (hook (list
               #'c-mode-common-hook
               #'c-mode-hook
               #'c++-mode-hook
               #'java-mode-hook
               #'haskell-mode-hook
               #'emacs-lisp-mode-hook
               #'lisp-interaction-mode-hook
               #'lisp-mode-hook
               #'maxima-mode-hook
               #'ielm-mode-hook
               #'sh-mode-hook
               #'makefile-gmake-mode-hook
               #'php-mode-hook
               #'python-mode-hook
               #'js-mode-hook
               #'go-mode-hook
               #'qml-mode-hook
               #'jade-mode-hook
               #'css-mode-hook
               #'ruby-mode-hook
               #'coffee-mode-hook
               #'rust-mode-hook
               #'qmake-mode-hook
               #'lua-mode-hook
               #'swift-mode-hook
               #'minibuffer-inactive-mode-hook
               #'typescript-mode-hook))
  (add-hook hook (lambda () (grammatical-edit-mode 1))))

(+define-keys! grammatical-edit
  (grammatical-edit-mode-map
    ((kbd "(") #'grammatical-edit-open-round)
    ((kbd "[") #'grammatical-edit-open-bracket)
    ((kbd "{") #'grammatical-edit-open-curly)
    ((kbd ")") #'grammatical-edit-close-round)
    ((kbd "]") #'grammatical-edit-close-bracket)
    ((kbd "}") #'grammatical-edit-close-curly)
    ((kbd "=") #'grammatical-edit-equal)
    ((kbd "%") #'grammatical-edit-match-paren)
    ((kbd "\"") #'grammatical-edit-double-quote)
    ((kbd "'") #'grammatical-edit-single-quote)
    ((kbd "SPC") #'grammatical-edit-space)
    ((kbd "RET") #'grammatical-edit-newline)
    ((kbd "M-o") #'grammatical-edit-backward-delete)
    ((kbd "C-d") #'grammatical-edit-forward-delete)
    ((kbd "C-k") #'grammatical-edit-kill)
    ((kbd "M-\"") #'grammatical-edit-wrap-double-quote)
    ((kbd "M-'") #'grammatical-edit-wrap-single-quote)
    ((kbd "M-[") #'grammatical-edit-wrap-bracket)
    ((kbd "M-{") #'grammatical-edit-wrap-curly)
    ((kbd "M-(") #'grammatical-edit-wrap-round)
    ((kbd "M-)") #'grammatical-edit-unwrap)
    ((kbd "M-p") #'grammatical-edit-jump-right)
    ((kbd "M-n") #'grammatical-edit-jump-left)
    ((kbd "M-:") #'grammatical-edit-jump-out-pair-and-newline)
    ((kbd "C-j") #'grammatical-edit-jump-up)))

(provide '+grammatical-edit)

;;; +grammatical-edit.el ends here
