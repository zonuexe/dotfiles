;;; init.el --- zonuexe's .emacs -*- coding: utf-8 ; lexical-binding: t -*-

;; Filename: init.el
;; Description: zonuexe's .emacs
;; Package-Requires: ((emacs "24.3"))
;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 2014-11-01
;; Modified: 2014-11-27
;; Version: 10.10
;; Keywords: internal, local
;; Human-Keywords: Emacs Initialization
;; Namespace: my/
;; URL: https://github.com/zonuexe/dotfiles/blob/master/.emacs.d/init.el

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
;;
;; Nobiscum Sexp. - S-expression is with us.
;;
;;; Code:

;;; Color-theme:
(load-theme 'manoj-dark t)

;;; Variables:

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq make-backup-files nil)
(setq delete-auto-save-files t)

;;; Font:
;;;     |いろはにほへと　ちりぬるを|
;;;     |わかよたれそ　　つねならむ|
;;;     |うゐのおくやま　けふこえて|
;;;     |あさきゆめみし　ゑひもせす|

(when (and window-system (>= emacs-major-version 23))
  (set-frame-font "Migu 2M-15.5"))

;;; Packages:

(when (require 'cask "~/.cask/cask.el" t)
  (cask-initialize))

(require 'use-package)
(use-package pallet
  :init (pallet-mode t))

(use-package nyan-mode
  :config
  (progn
    (custom-set-variables
     '(nyan-bar-length 16))
    (nyan-mode t)))

;;; Environment:

;; PATH
(use-package exec-path-from-shell
  :config
  (progn
    (exec-path-from-shell-initialize)))

;;; Coding:
(setq-default indent-tabs-mode nil)

;; White space
(setq-default show-trailing-whitespace t)

;; Uniquify
(use-package uniquify
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Show paren
(show-paren-mode t)

;; Column mode
(column-number-mode t)

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

(use-package sequential-command
  :config
  (progn
    (define-sequential-command my/seq-home
      beginning-of-line beginning-of-line beginning-of-defun beginning-of-buffer seq-return)
    (define-sequential-command my/seq-end
      end-of-line end-of-line end-of-defun end-of-buffer seq-return)
    (bind-key "C-a" 'my/seq-home)
    (bind-key "C-e" 'my/seq-end)))

;; Helm
(use-package helm
  :config
  (progn
    (require 'helm-config)
    (helm-mode t)))

(use-package helm-gtags
  :config
  (progn
    (bind-key "M-."   'helm-gtags-find-tag  helm-gtags-mode-map)
    (bind-key "C-M-." 'helm-gtags-find-rtag helm-gtags-mode-map)))

(use-package helm-ag)

;; Auto-Complete
(use-package auto-complete
  :config
  (progn
    (add-to-list 'ac-dictionary-directories (locate-user-emacs-file "./ac-dict"))
    (require 'auto-complete-config)
    (ac-config-default)
    (global-auto-complete-mode t)))

;; Magit
(use-package magit
  :config
  (progn
    (setq vc-handled-backends '())
    (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
    (bind-key "C-x m" 'magit-status)
    (bind-key "C-c l" 'magit-blame-mode)))

;; Projectile
(use-package projectile
  :config
  (progn
    (use-package helm-projectile)
    (projectile-global-mode t)))

;; Flycheck
(use-package flycheck
  :config
  (progn
    (global-flycheck-mode t)))

;; Smartparens
(use-package smartparens
  :config
  (progn
    (use-package smartparens-config)
    (smartparens-global-mode t)))

;; smartchr
(use-package smartchr)

;;; Languages:

;; PHP
(require 'cl)
(use-package php-mode
  :config
  (progn
    (use-package php-auto-yasnippets)
    (defun my/php-mode-hook ()
      (subword-mode t)
      (payas/ac-setup))
    (bind-key "[" (smartchr "[]" "array()" "[[]]") php-mode-map)
    (bind-key "]" (smartchr "array " "]" "]]")     php-mode-map)
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

(use-package inf-ruby
  :config
  (progn
    (custom-set-variables
     '(inf-ruby-default-implementation "pry")
     '(inf-ruby-eval-binding "Pry.toplevel_binding"))
    (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)))

;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; Lisp
(defvar my/elisp-mode-hooks
      '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook))
(--each my/elisp-mode-hooks (add-hook it 'turn-on-eldoc-mode))

(use-package paredit
  :config
  (progn
    (bind-key "C-<right>" 'right-word paredit-mode-map)
    (bind-key "C-<left>"  'left-word  paredit-mode-map)
    (--each my/elisp-mode-hooks (add-hook it 'enable-paredit-mode))))

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

;;; Others:

;; Recentf
(use-package recentf
  :config
  (progn
    (recentf-mode t)
    (bind-key "C-c r" 'helm-recentf)))

;; Undo Tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; expand-region.el
(use-package expand-region
  :config
  (progn
    (bind-key "C-@"   'er/expand-region)
    (bind-key "C-M-@" 'er/contract-region)))

;;; Tools:

;; term+
(use-package term+
  :config
  (progn
    (use-package term+key-intercept)
    (use-package term+mux)
    (require 'xterm-256color)))

;; Open junk file
(use-package open-junk-file
  :config
  (progn
    (setq open-junk-file-format "~/junk/%Y/%m/%Y-%m-%d-%H%M%S.")
    (bind-key "C-c j" 'open-junk-file)))

;; w3m
(use-package w3m)

;; navi2ch
(use-package navi2ch
  :config
  (progn
    (use-package navi2ch-mona)
    (custom-set-variables
     '(navi2ch-article-use-jit t)
     '(navi2ch-article-exist-message-range nil)
     '(navi2ch-article-new-message-range nil)
     '(navi2ch-mona-enable t)
     '(navi2ch-mona-use-ipa-mona t)
     '(navi2ch-mona-ipa-mona-font-family-name "mona-izmg16"))
    (navi2ch-mona-setup)))

;; ElScreen
(use-package elscreen
  :config
  (progn
    (custom-set-variables
     '(elscreen-prefix-key (kbd "C-t"))
     '(elscreen-display-tab nil)
     '(elscreen-tab-display-kill-screen nil)
     '(elscreen-tab-display-control nil))
    (elscreen-start)
    (bind-key "C-t p" 'helm-elscreen)))

;; Calfw
(use-package calfw)

;; moccur
(use-package color-moccur)
(use-package moccur-edit)

;; dired-k
(use-package direx)
(use-package dired-k
  :config
  (progn
    (add-hook 'dired-initial-position-hook 'dired-k)
    (bind-key "K" 'dired-k dired-mode-map)
    (bind-key "M-C-\\" 'direx-project:jump-to-project-root-other-window)
    (bind-key "M-C-¥"  'direx-project:jump-to-project-root-other-window)))

;; Wdired
(use-package wdired)

;; UCS Utility
(use-package ucs-utils)

;;; Games:
(use-package gnugo)

;;; Server:
(use-package edit-server
  :if window-system
  :init
  (progn
    (add-hook 'after-init-hook 'server-start t)
    (add-hook 'after-init-hook 'edit-server-start t)))

;;; Variables:
(defvar my/hidden-minor-modes
      '(undo-tree-mode
        eldoc-mode
        auto-complete-mode
        magit-auto-revert-mode
        abbrev-mode
        smartparens-mode
        helm-mode
        helm-gtags-mode))
(--each my/hidden-minor-modes
  (setq minor-mode-alist
        (cons (list it "") (assq-delete-all it minor-mode-alist))))

;;; init.el ends here
