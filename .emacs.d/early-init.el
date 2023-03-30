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

(tool-bar-mode -1)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; (setq package-enable-at-startup nil)

(with-eval-after-load 'package
  (when (eval-when-compile (version< emacs-version "28.0.50"))
    (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(custom-set-variables
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))

(eval-when-compile
  (unless (> emacs-major-version 28)
    (put 'diff-add-log-use-relative-names 'safe-local-variable #'booleanp)
    (put 'vc-git-annotate-switches 'safe-local-variable (lambda (switches) (equal switches "-w")))))

;;; early-init.el ends here
