;;; pixiv-dev.el --- p(ixi)v -*- coding: utf-8 ; lexical-binding: t -*-

;; Copyright (C) 2016 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 2016-04-01
;; Modified: 2016-03-07
;; Version: 1.0.0
;; Keywords: processes tools php
;; Package-Requires: ((emacs "24.3") (flycheck "1") (psysh "0.0.4"))
;; URL: https://github.com/zonuexe/dotfiles/tree/master/.emacs.d/site-lisp

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(require 'psysh nil t)
(require 'flycheck)

(defgroup pixiv-dev '()
  "Develop pixiv.net and other services."
  :group 'programming)

(defcustom pixiv-dev-user-name user-login-name
  "Login name for pixiv-dev(LDAP) or E-mail address.")

(defcustom pixiv-dev-host "pixiv-dev"
  "Host name of your pixiv develop server.")

(defcustom pixiv-dev-working-dir nil
  "`pixiv.git' working directory.")

(defvar pixiv-dev-repository-web "http://gitlab.pixiv.private/pixiv/pixiv"
  "URL of `pixiv.git' repository web.")

(defvar pixiv-dev-psysh-buffer-process
  '("pixiv-shell" "dev-script/shell.php"))

(defun pixiv-dev--working-dir ()
  "Wokring directory of `pixiv.git'."
  (or pixiv-dev-working-dir
      (format "/scp:%s:/mnt/ssd1/home/%s/pixiv/" pixiv-dev-host pixiv-dev-user-name)))

;; Flycheck

;;;###autoload
(flycheck-define-checker pixiv-dev-lint
  "Lint for pixiv.git"
  :command ("pixiv-lint" source)
  :error-patterns
  ((info line-start "file:" (file-name) "\tline:" line "\tcol:" (+ (or "-" num))
         "\tlevel:info" "\tdesc:" (message) line-end)
   (error line-start "file:" (file-name) "\tline:" line "\tcol:" (+ (or "-" num))
          "\tlevel:error" "\tdesc:" (message) line-end)
   (warning line-start "file:" (file-name) "\tline:" line "\tcol:" (+ (or "-" num))
            "\tlevel:" (+ alnum) "\tdesc:" (message) line-end))
  :modes (php-mode web-mode)
  :next-checkers (php))
;; (flycheck-select-checker 'pixiv-dev-lint)
;; flycheck-pixiv-dev-lint-executable

;; Utillity

;;;###autoload
(defun pixiv-dev-copy-file-url ()
  "Copy pixiv repository file URL."
  (interactive)
  (let (path (working-dir (pixiv-dev--working-dir)))
    (if (or (null buffer-file-name)
            (not (string-prefix-p working-dir buffer-file-name)))
        (error "File is not in pixiv repository!")
      (setq path (concat pixiv-dev-repository-web "/tree/master/"
                         (replace-regexp-in-string working-dir "" buffer-file-name)
                         "#L" (number-to-string (1+ (count-lines 1 (point))))))
      (kill-new path)
      (message (format "Copy `%s'!" path)))))

;;;###autoload
(defun pixiv-dev-find-file ()
  "Find file in pixiv working directory."
  (interactive)
  (let ((default-directory (pixiv-dev--working-dir)))
    (call-interactively 'find-file nil)))

;;;###autoload
(defun pixiv-dev-shell ()
  "Run PHP interactive shell for pixiv."
  (interactive)
  (let ((default-directory (pixiv-dev--working-dir))
        buffer)
    (if (fboundp 'psysh-mode)
        (apply #'psysh-run pixiv-dev-psysh-buffer-process)
      (setq buffer (make-comint "pixiv-shell" "dev-script/shell.php"))
      (switch-to-buffer buffer))))

;; pixiv-dev-mode
(defvar pixiv-dev-mode-lighter " p(ixi)v")

(defvar pixiv-dev-mode-map
  (let ((map (make-sparse-keymap)))
    ;;
    map))

;;;###autoload
(define-minor-mode pixiv-dev-mode
  "Minor mode for editing pixiv PHP project."
  nil pixiv-dev-mode-lighter pixiv-dev-mode-map
  (setq psysh-comint-buffer-process pixiv-dev-psysh-buffer-process)
  (let ((path-to-tags (concat (pixiv-dev--working-dir) "TAGS")))
    (when (file-exists-p path-to-tags)
      ;(setq-local tags-file-name path-to-tags)
      ))
  (when (or flycheck-pixiv-dev-lint-executable (executable-find "pixiv-lint"))
    (flycheck-select-checker 'pixiv-dev-lint)))

;;;###autoload
(defun pixivision ()
  "Visit pixivision on W3M."
  (interactive)
  (w3m-browse-url "http://www.pixivision.net"))

(provide 'pixiv-dev)
;;; pixiv-dev.el ends here
