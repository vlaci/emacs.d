;;; +lib.el --- Common utilities for my Emacs configuration -*- lexical-binding: t -*-

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

(require '+config)
(require 'set-defaults)
(require 'cl-lib)

;;;###autoload
(defmacro +set-defaults! (&rest pairs)
  "Override default of `defcustom' variable VAR to VALUE from PAIRS.

\(fn [VAR VALUE]...)"
  (declare (debug setq))
  (unless (zerop (mod (length pairs) 2))
    (error "PAIRS must have an even number of var/value members"))
  (let ((pairs (cl-loop for (k v) on pairs by #'cddr
                        collect `'(,k ,v))))
    `(set-defaults ,@pairs)))

;;;###autoload
(defmacro +after-gui! (&rest body)
  "Run BODY once after the first GUI frame is created."
  (declare (indent 0) (debug t))
  `(let ((hook (if (daemonp)
                   'server-after-make-frame-hook
                 'after-init-hook)))
     (general-add-hook hook
                       (lambda () ,@body)
                       nil
                       nil
                       t)))

;;;###autoload
(defmacro +after! (feature &rest body)
  (declare (indent defun)(debug t))
  (when (bound-and-true-p byte-compile-current-file)
                (require feature nil 'noerror))
  `(eval-after-load ',feature ',(macroexp-progn body)))

(defsubst +normalize-direction (direction)
  (cond
   ((memq direction '(left leftmost)) '(left . right))
   ((memq direction '(right rightmost)) '(right . left))
   ((memq direction '(above top up)) '(above . below))
   ((memq direction '(below bottom down)) '(below . above))
   (t '(below above))))

;;;###autoload
(defun +display-buffer-in-direction (buffer alist)
  "Try to display BUFFER at edge specified in ALIST."
  (let* ((direction (alist-get 'direction alist))
         (selected-window (selected-window))
         (reference-window (+find-window direction))
         (should-split (or (eq reference-window selected-window)
                           (>= (window-width reference-window) split-width-threshold)))
         (nd (+normalize-direction direction))
         (window-pos (car nd))
         (split-dir (if (eq window-pos direction) (cdr nd) window-pos))
         (window (if should-split
                     (split-window (frame-root-window) nil split-dir)
                   ;; TODO: delete according to direction
                   (delete-other-windows-vertically reference-window)
                   reference-window)))
    (window--display-buffer buffer window (if should-split 'window 'reuse) alist)
    (when should-split
      (set-window-prev-buffers window nil))))

(defun +find-window (direction &optional window)
  "Find the window at the given DIRECTION starting from WINDOW or current window."
  (let* ((edge '(leftmost rightmost top bottom))
         (window (or window (selected-window)))
         (nd (car (+normalize-direction direction)))
         (new-window (window-in-direction nd window)))
    (cond ((and new-window (memq direction edge))
           (+find-window direction new-window))
          (new-window new-window)
          (t window))))

(defun +advised-executable-find (executable-find-fn &rest args)
  (let ((exec-path (append exec-path +extra-exec-path)))
    (setenv "PATH" (mapconcat #'identity exec-path path-separator))
    (apply executable-find-fn args)))

(advice-add 'executable-find :around #'+advised-executable-find)

(provide '+core)

;;; +core.el ends here
