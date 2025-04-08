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
(require 'hi-lock)

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

(eval-when-compile
  (require 'composer)
  (require 'php))
(declare-function php-in-string-or-comment-p "ext:php" ())
(declare-function php-in-string-p "ext:php" ())
(declare-function c-backward-token-2 "cc-engine" (&optional count balanced limit))
(declare-function smartchr-make-struct "ext:smartchr" (_key1 cleanup-fn _key2 insert-fn))
(declare-function composer--find-composer-root "ext:composer" (directory))
(declare-function hi-lock-face-phrase-buffer "hi-lock" (regexp &optional face))

;;;###autoload
(defun my-php-smartchr-dot (no-string within-string-or-comment next-to-string)
  "Make a smartchr to press `.` key in PHP with NO-STRING.
WITHIN-STRING-OR-COMMENT, NEXT-TO-STRING."
  (let ((select-template
         (lambda ()
           (save-excursion
             (cond
              ((php-in-string-or-comment-p) within-string-or-comment)
              ((and (c-backward-token-2 1 nil)
                    (goto-char (1+ (point)))
                    (or (php-in-string-p)
                        (let* ((at-point-string (thing-at-point 'symbol t)))
                          (member at-point-string php-magical-constants))))
               next-to-string)
              (no-string))))))
    (smartchr-make-struct
     :cleanup-fn (lambda () (delete-char (- (length (funcall select-template)))))
     :insert-fn (lambda () (insert (funcall select-template))))))

(defun my-php-parse-this-buffer ()
  ""
  (interactive)
  (let* ((default-directory (or (composer--find-composer-root default-directory)
                                (getenv "HOME")))
         (dirs (list #'composer-get-bin-dir
                     (lambda () (let ((composer-global-command t))
                                  (composer-get-bin-dir)))))
         (path (cl-loop for dir in dirs
                        for path = (expand-file-name "php-parse" (funcall dir))
                        if (file-exists-p path)
                        return path)))
    (compile (mapconcat #'shell-quote-argument
                        (mapcar #'file-relative-name (list path (buffer-file-name)))
                        " "))))

(defun my-browse-url-wsl-host-browser (url &rest _args)
  "Browse URL with WSL host web browser."
  (prog1 (message "Open %s" url)
    (shell-command-to-string
     (mapconcat #'shell-quote-argument
                (list "cmd.exe" "/c" "start" url)
                " "))))

(require 'dired)

(defconst my-system-is-wsl2
  (eval-when-compile
    (getenv "WSL_DISTRO_NAME")))

(defun my-wsl-convert-path (path)
  "Browse URL with WSL host web browser."
  (interactive "P")
  (let* ((win-path (replace-regexp-in-string
                    (regexp-opt (list "/"))
                    "\\\\"
                    path)))
    (format "\\\\wsl.localhost\\%s%s" my-system-is-wsl2 win-path)))

(defun my-wsl-open-with (_arg)
  "Browse URL with WSL host web browser."
  (interactive "P")
  (when my-system-is-wsl2
    (let* ((current-file-name
            (if (derived-mode-p 'dired-mode)
                (dired-get-file-for-visit)
              buffer-file-name)))
      (shell-command-to-string
       (mapconcat #'shell-quote-argument
                  (list "cmd.exe" "/c" "start" current-file-name)
                  " ")))))

(defun my-uchizei (total tax)
  "総額(TOTAL)と税率(TAX)から内税を計算する."
  (interactive (list (read-number "総額: ")
                     (let* ((tax-rates '(("10%" . 10) ("8% (軽減税率)" . 8)))
                            (value (completing-read "消費税率: " tax-rates)))
                       (cdr (assoc value tax-rates)))))
  (let ((result (/ (* total tax) (+ 100 tax))))
    (kill-new (number-to-string result))
    (message "消費税額: %f" result)))

(defun my-highlight-phrase  (regexp &optional face)
  "Set face of each match of phrase REGEXP to FACE."
  (interactive
   (list
    (hi-lock-regexp-okay
     (read-regexp "Phrase to highlight"
                  (rx-to-string
                   (list : (if (region-active-p)
                               (buffer-substring-no-properties (region-beginning) (region-end))
                             (thing-at-point 'symbol)))
                   t)
                  'regexp-history-last))
    (hi-lock-read-face-name)))
  (highlight-phrase regexp face))

(defun my-tetris-before ()
  "Advice function for `tetris' command."
  (browse-url "https://www.youtube.com/watch?v=Soy4jGPHr3g"))

(advice-add 'tetris :before #'my-tetris-before)

;;;###autoload
(defmacro compact (&rest variables)
  "Compact VARIABLES into plist."
  (cons 'list
        (cons (quote 'compact)
              (cl-loop for var in variables
                       nconc (list (intern (format ":%s" var)) var)))))

;;;###autoload
(defun my-invoice-search (regnums)
  "国税庁適格請求書発行事業者公表サイトでリージョン内の登録番号 REGNUMS を検索する."
  (interactive (list (cl-remove-if-not
                      (lambda (line) (string-prefix-p "T" line))
                      (split-string (string-trim (buffer-substring-no-properties (region-beginning) (region-end))) "\n"))))
  (browse-url (concat "https://www.invoice-kohyo.nta.go.jp/regno-search?"
                      (string-join (cl-mapcar (lambda (regnum n) (format "regNo%d=%s" n (string-trim-left regnum "T")))
                                              regnums (number-sequence 1 (length regnums)))
                                   "&"))))

(defun my-combinations-with-repetition (set n)
  "集合 SET から N 要素の重複配列を構築する."
  (if (zerop n)
      '(())
    (mapcan (lambda (x)
              (mapcar (lambda (xs) (cons x xs))
                      (my-combinations-with-repetition set (1- n))))
            set)))

(defun my-chunk-list (n lst)
  "リスト LST を最大 N 要素ごとのチャンクに分割する."
  (when lst
    (cons (cl-subseq lst 0 (min n (length lst)))
          (my-chunk-list n (nthcdr n lst)))))

(provide 'my)
;;; my.el ends here
