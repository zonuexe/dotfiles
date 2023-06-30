;;; my.el --- My private Lisp functions              -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Megurine Luka

;; Author: Megurine Luka <megurine@tadsan.local>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
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

;; ヾ(〃＞＜)ﾉﾞ☆

;;; Code:

;;;###autoload
(defun my-vterm-setup ()
  "Setup vterm-mode."
  (setq show-trailing-whitespace nil))

;;;###autoload
(defun my-filename-upto-parent ()
  "Move to parent directory like \"cd ..\" in find-file."
  (interactive)
  (let ((sep (eval-when-compile (regexp-opt '("/" "\\")))))
    (save-excursion
      (left-char 1)
      (when (looking-at-p sep)
        (delete-char 1)))
    (save-match-data
      (when (search-backward-regexp sep nil t)
        (right-char 1)
        (filter-buffer-substring (point)
                                 (save-excursion (end-of-line) (point))
                                 #'delete)))))

;;;###autoload
(defun my-consult-line (&optional initial start)
  "Wrapper function of `consult-line'."
  (interactive (list
                (when (use-region-p)
                  (prog1
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (deactivate-mark)))
                (not (not current-prefix-arg))))
  (consult-line initial start))

(declare-function php-in-string-or-comment-p "ext:php" ())
(declare-function php-in-string-p "ext:php" ())
(declare-function c-backward-token-2 "cc-engine" (&optional count balanced limit))
(declare-function smartchr-make-struct "ext:smartchr" (_key1 cleanup-fn _key2 insert-fn))

;;;###autoload
(defun my-php-smartchr-dot (no-string within-string-or-comment next-to-string)
  "Make a smartchr to press `.` key in PHP with NO-STRING, WITHIN-STRING-OR-COMMENT and NEXT-TO-STRING."
  (let ((select-template
         (lambda ()
           (save-excursion
             (cond
              ((php-in-string-or-comment-p) within-string-or-comment)
              ((and (c-backward-token-2 1 nil)
                    (goto-char (1+ (point)))
                    (php-in-string-p))
               next-to-string)
              (no-string))))))
    (smartchr-make-struct
     :cleanup-fn (lambda () (delete-char (- (length (funcall select-template)))))
     :insert-fn (lambda () (insert (funcall select-template))))))

(provide 'my)
;;; my.el ends here
