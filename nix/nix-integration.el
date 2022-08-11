;;; nix-integration.el --- summary -*- lexical-binding: t -*-

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

(setq-default ispell-program-name "@hunspell@/bin/hunspell")
(setq-default langtool-java-bin "@jre@/bin/java"
              langtool-language-tool-jar "@languagetool@/share/languagetool-commandline.jar"
              mu4e-binary "@mu@/bin/mu"
              sendmail-program "@msmtp@/bin/msmtp")
(setq-default +eglot-pyright-executable "@pyright@/bin/pyright-langserver"
              +eglot-rust-analyzer-executable "@rustanalyzer@/bin/rust-analyzer")

(provide 'nix-integration)

;;; nix-integration.el ends here
