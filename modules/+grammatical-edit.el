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
               #'python-ts-mode-hook
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

(general-define-key
 :keymaps 'grammatical-edit-mode-map
 "(" #'grammatical-edit-open-round
 "[" #'grammatical-edit-open-bracket
 "{" #'grammatical-edit-open-curly
 "" #'grammatical-edit-close-round
 "]" #'grammatical-edit-close-bracket
 "}" #'grammatical-edit-close-curly
 "=" #'grammatical-edit-equal
 "%" #'grammatical-edit-match-paren
 "\"" #'grammatical-edit-double-quote
 "'" #'grammatical-edit-single-quote
 "SPC" #'grammatical-edit-space
 "RET" #'grammatical-edit-newline
 "M-o" #'grammatical-edit-backward-delete
 "C-d" #'grammatical-edit-forward-delete
 "C-k" #'grammatical-edit-kill
 "M-\"" #'grammatical-edit-wrap-double-quote
 "M-'" #'grammatical-edit-wrap-single-quote
 "M-[" #'grammatical-edit-wrap-bracket
 "M-{" #'grammatical-edit-wrap-curly
 "M-(" #'grammatical-edit-wrap-round
 "M-" #'grammatical-edit-unwrap
 "M-p" #'grammatical-edit-jump-right
 "M-n" #'grammatical-edit-jump-left
 "M-:" #'grammatical-edit-jump-out-pair-and-newline
 "C-j" #'grammatical-edit-jump-up)

(provide '+grammatical-edit)

;;; +grammatical-edit.el ends here
