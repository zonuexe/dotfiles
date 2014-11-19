;;; init.el --- zonuexe's .emacs -*- Coding: utf-8 ; lexical-binding: t -*-

;; Filename: init.el
;; Description: zonuexe's .emacs
;; Package-Requires: ((emacs "24.3"))
;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 2014-11-01
;; Modified: 2014-11-18
;; Version: 10.10
;; Keywords: internal, local
;; Human-Keywords: Emacs Initialization
;; Namespace: my/
;; URL: https://github.com/zonuexe/dotfiles/.emacs.d/init.el

(load-theme 'manoj-dark t)

;;; Variables:

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

(custom-set-variables
  '(init-loader-show-log-after-init t)
  '(inhibit-startup-message nil))

;;; Font:
;;;     |いろはにほへと　ちりぬるを|
;;;     |わかよたれそ　　つねならむ|
;;;     |うゐのおくやま　けふこえて|
;;;     |あさきゆめみし　ゑひもせす|

(when (and window-system (>= emacs-major-version 23))
  (set-default-font "Migu 2M-15.5"))

;;; Packages:

(when (require 'cask "~/.cask/cask.el" t)
  (cask-initialize))

(require 'use-package)
(use-package pallet
  :init (pallet-mode t))

;;; Environment:

;; PATH
(use-package exec-path-from-shell
  :config
  (progn
    (exec-path-from-shell-initialize)))

;;; Coding:

;; White space
(use-package whitespace-mode
  :config
  (progn
    (custom-set-variables
     '(whitespace-style (lines-tail))
     '(whitespace-space-regexp "\\(\u3000+\\)"))
    (global-whitespace-mode t)))

;; Show paren
(show-paren-mode t)

;; Key config
(use-package bind-key
  :config
  (progn
    (bind-key  "M-ESC ESC"   'keyboard-quit)
    (bind-key  "C-c R"       'revert-buffer)
    (bind-key* "C-c <left>"  'windmove-left)
    (bind-key* "C-c <down>"  'windmove-down)
    (bind-key* "C-c <up>"    'windmove-up)
    (bind-key* "C-c <right>" 'windmove-right))
  (cond
   ((eq window-system 'ns)
    (global-set-key (kbd "M-¥") (lambda () (interactive) (insert "¥")))
    (global-set-key (kbd "¥")   (lambda () (interactive) (insert "\\"))))))

;; Helm
(use-package helm
  :config
  (progn
    (require 'helm-config)
    (helm-mode t)))

(use-package helm-gtags
  :config
  (progn
    (bind-key "M-." 'helm-gtags-find-tag helm-gtags-mode-map)
    (bind-key "C-M-." 'helm-gtags-find-rtag helm-gtags-mode-map)))

;; Auto-Complete
(use-package auto-complete
  :config
  (progn
    (add-to-list 'ac-dictionary-directories (locate-user-emacs-file "./ac-dict"))
    (require 'auto-complete-config)
    (ac-config-default)))

;; Magit
(use-package magit
  :config
  (progn
    (bind-key "C-x m" 'magit-status)
    (bind-key "C-c l" 'magit-blame-mode)))

;; Flycheck
(use-package flycheck)

;;; Languages:

;; PHP
(require 'cl)
(use-package php-mode
  :config
  (progn
    (defun my/php-mode-hook ()
      )
    (add-hook 'php-mode-hook 'my/php-mode-hook)
    (add-hook 'php-mode-hook 'helm-gtags-mode)))

;; Ruby
(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :config
  (progn
    (use-package rhtml-mode)
    (setq-default enh-ruby-not-insert-magic-comment t)))

;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; Lisp
(use-package paredit
  :config
  (progn
    (bind-key "C-<right>" 'right-word paredit-mode-map)
    (bind-key "C-<left>"  'left-word  paredit-mode-map)
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-interacton-mode-hook 'enable-paredit-mode)))

;; Scala
(use-package scala-mode2
  :config
  (use-package ensime)
  :init
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

;; JSON
(use-package json-mode)

;; YAML
(use-package yaml-mode)

;; Markdown Mode
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

;;; Recentf
(use-package recentf
  :config
  (progn
    (recentf-mode t)
    (bind-key "C-c r" 'helm-recentf)
    (bind-key "C-c C-r" 'helm-recentf)))

;;; Undo Tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;;; Tools:

;; term+
(use-package term+
  :config
  (progn
    (use-package term+key-intercept)
    (require 'xterm-256color)))

;; Open junk file
(use-package open-junk-file
  :config
  (progn
    (setq open-junk-file-format "~/junk/%Y/%m/%Y-%m-%d-%H%M%S.")
    (bind-key "C-c j" 'open-junk-file)))

;; w3m
(use-package w3m)

;;; Server:
(use-package edit-server
  :if window-system
  :init
  (progn
    (add-hook 'after-init-hook 'server-start t)
    (add-hook 'after-init-hook 'edit-server-start t)))
