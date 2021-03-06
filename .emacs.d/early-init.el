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

(with-eval-after-load 'package
  (when (eval-when-compile (version< emacs-version "28.0.50"))
    (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

;;; early-init.el ends here
