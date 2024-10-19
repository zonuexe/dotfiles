;;; early-init --- Early initialization file  -*- lexical-binding: t; -*-

;;; Commentary:

;; 三三三 ヾ(〃＞＜)ﾉﾞ☆

;;; Code:
(eval-when-compile
  (require 'nadvice))

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

(defconst my-system-is-wsl2
  (eval-when-compile
    (getenv "WSL_DISTRO_NAME")))

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(custom-set-variables
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))

(unless (eval-when-compile (>= emacs-major-version 30))
  (put 'etags-regen-regexp-alist 'safe-local-variable #'listp)
  (put 'etags-regen-ignores 'safe-local-variable #'listp)
  emacs-major-version)

(advice-add #'treesit-available-p :override (lambda () nil))

(when (eval-when-compile (eq system-type 'darwin))
  (setq read-process-output-max 65536))

;;; early-init.el ends here
