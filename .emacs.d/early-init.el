;;; early-init --- Early initialization file  -*- lexical-binding: t; -*-

;;; Commentary:

;; 三三三 ヾ(〃＞＜)ﾉﾞ☆

;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq use-dialog-box nil)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;; Packages:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(let ((default-directory (locate-user-emacs-file "./el-get")))
  (normal-top-level-add-subdirs-to-load-path))

(when (eval-when-compile (version< emacs-version "27"))
  (package-initialize))

;;; early-init.el ends here
