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

(setq gc-cons-threshold (* 1024 1024 1024))

(if window-system
    (tool-bar-mode -1)
  (menu-bar-mode -1))

;;; Color-theme:
(defvar my/load-themes '(manoj-dark tango))
(load-theme (car my/load-themes) t)

;;; Variables:

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq make-backup-files nil)
(setq delete-auto-save-files t)

(add-to-list 'load-path (locate-user-emacs-file "./site-lisp"))

;;; Font:
;;;     |いろはにほへと　ちりぬるを|
;;;     |わかよたれそ　　つねならむ|
;;;     |うゐのおくやま　けふこえて|
;;;     |あさきゆめみし　ゑひもせす|

(defconst my/default-display-font-family "Migu 2M")
(defconst my/display-font-size-by-hostname-alist
  '(("MegurineUbu1410"  . 12.5)
    ("tadsan-ret.local" . 17.5)))

(when (and window-system (>= emacs-major-version 23))
  (set-frame-font
   (format "%s-%.1f"
           my/default-display-font-family
           (or (cdr (assoc (system-name) my/display-font-size-by-hostname-alist))
               15.5))))

;;; Packages:
(when (or (require 'cask "~/.cask/cask.el" t)
	  (require 'cask nil t))
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
    (custom-set-variables
      '(exec-path-from-shell-variables ("PATH" "MANPATH" "GOROOT" "GOPATH")))
    (exec-path-from-shell-initialize)))

;;; Coding:
(setq-default indent-tabs-mode nil)

;; White space
(setq-default show-trailing-whitespace t)

;; Uniquify
(use-package uniquify
  :config
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
    (bind-key  "C-x お"      'other-window)
    (bind-key  "M-："        'eval-expression)
    (bind-key  "M-ESC ："    'eval-expression)
    (bind-key* "C-c <left>"  'windmove-left)
    (bind-key* "C-c <down>"  'windmove-down)
    (bind-key* "C-c <up>"    'windmove-up)
    (bind-key* "C-c <right>" 'windmove-right))
  (cond
   ((eq window-system 'ns)
    (--each '(ns-command-modifier ns-alternate-modifier)
      (when (boundp it) (set it 'meta)))
    (bind-key "M-¥" (lambda () (interactive) (insert "¥")))
    (bind-key "¥"   (lambda () (interactive) (insert "\\"))))
   ((eq window-system 'x)
    (--each '(x-meta-keysym x-super-keysym)
      (when (boundp it) (set it 'meta))))))

(use-package key-chord
  :init
  (progn
    (custom-set-variables
     '(key-chord-two-keys-delay 0.05))
    (key-chord-mode 1)
    (key-chord-define-global "df" 'find-function)
    (key-chord-define-global "ip" 'package-install)
    (key-chord-define-global "kl" 'align-regexp)
    (key-chord-define-global "rt" 'toggle-load-theme)
    (key-chord-define-global "m," 'reload-major-mode)))

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
(use-package helm :defer t
  :config
  (progn
    (require 'helm-config)
    (helm-mode t)))

(use-package helm-ag :defer t)

;; Auto-Complete
(use-package auto-complete
  :config
  (progn
    (add-to-list 'ac-dictionary-directories (locate-user-emacs-file "./ac-dict"))
    (require 'auto-complete-config)
    (ac-config-default)
    (global-auto-complete-mode t)))

;; Magit
(use-package magit :defer t
  :init
  (progn
    (setq-default magit-last-seen-setup-instructions "1.4.0")
    (setq-default magit-auto-revert-mode nil)
    (setq vc-handled-backends '())
    (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
    (bind-key "C-x m" 'magit-status)
    (bind-key "C-c l" 'magit-blame-mode)))

;; Projectile
(use-package projectile
  :config
  (progn
    (use-package helm-projectile)
    (custom-set-variables
     '(projectile-completion-system 'helm))
    (projectile-global-mode t)
    (helm-projectile-on)
    (add-hook 'projectile-mode-hook 'projectile-rails-on)))

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
(use-package smartchr :defer t
  :commands smartchr)

;; smart-newline
(use-package smart-newline :defer t
  :init
  (progn
    (bind-key "C-m" 'smart-newline)))

;; YASnippets
(use-package yasnippet
  :init (yas-global-mode t))

;;; Languages:

;; PHP
(use-package php-mode :defer t
  :config
  (progn
    (use-package php-auto-yasnippets)
    (defun my/php-mode-hook ()
      (when (require 'php-eldoc nil t)
        (php-eldoc-enable))
      (subword-mode t)
      (setq show-trailing-whitespace t)
      (c-set-style "psr2")
      (payas/ac-setup))
    (custom-set-variables
     '(php-refactor-keymap-prefix (kbd "C-c v")))
    (bind-key "[" (smartchr "[]" "array()" "[[]]") php-mode-map)
    (bind-key "]" (smartchr "array " "]" "]]")     php-mode-map)
    (bind-key "C-c C-y" 'yas/create-php-snippet    php-mode-map)
    (add-hook 'php-mode-hook 'my/php-mode-hook)
    (add-hook 'php-mode-hook 'php-refactor-mode)))

;; Ruby
(use-package enh-ruby-mode :defer t
  :mode (("\\.rb\\'"   . enh-ruby-mode)
         ("\\.rake\\'" . enh-ruby-mode))
  :interpreter "pry"
  :config
  (progn
    (use-package robe)
    (defun my/enh-ruby-mode-hook ()
      (set (make-local-variable 'ac-ignore-case) t))
    (add-to-list 'ac-modes 'enh-ruby-mode)
    (custom-set-variables
     '(ruby-deep-indent-paren-style nil))
    (setq-default enh-ruby-not-insert-magic-comment t)
    (add-hook 'robe-mode-hook 'ac-robe-setup)))

;;; begin enh-ruby-mode patch
;;; http://qiita.com/vzvu3k6k/items/acec84d829a3dbe1427a
(defadvice enh-ruby-mode-set-encoding (around stop-enh-ruby-mode-set-encoding)
  "If enh-ruby-not-insert-magic-comment is true, stops enh-ruby-mode-set-encoding."
  (if (and (boundp 'enh-ruby-not-insert-magic-comment)
           (not enh-ruby-not-insert-magic-comment))
      ad-do-it))
(ad-activate 'enh-ruby-mode-set-encoding)
(setq-default enh-ruby-not-insert-magic-comment t)
;;; enh-ruby-mode patch ends here

;; rhtml
(use-package rhtml-mode :defer t)

;; inf-ruby
(use-package inf-ruby :defer t
  :config
  (progn
    (custom-set-variables
     '(inf-ruby-default-implementation "pry")
     '(inf-ruby-eval-binding "Pry.toplevel_binding"))
    (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)))

;; Python
(use-package python :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; Lisp
(defvar my/lisp-mode-hooks
      '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook))
(--each my/lisp-mode-hooks (add-hook it 'turn-on-eldoc-mode))

(use-package lispxmp :defer t
  :init
  (progn
    (bind-key "C-c C-e" 'lispxmp emacs-lisp-mode-map)))

(use-package paredit :defer t
  :init
  (--each my/lisp-mode-hooks (add-hook it 'enable-paredit-mode))
  :config
  (progn
    (bind-key "C-<right>" 'right-word paredit-mode-map)
    (bind-key "C-<left>"  'left-word  paredit-mode-map)))

(use-package sly :defer t
  :init
  (progn
    (require 'sly-autoloads))
  (custom-set-variables
   '(inferior-lisp-program "sbcl")))

(use-package haskell-mode :defer t
  :init
  (add-hook 'haskell-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

;; Scala
(use-package scala-mode2 :defer t
  :init
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  :config
  (use-package ensime))

;; JavaScript
(use-package js2-mode :defer t
  :mode "\\.js\\'")

;; TypeScript
(use-package typescript :defer t
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (progn
    (use-package tss)
    (custom-set-variables
     '(tss-popup-help-key "C-:")
     '(tss-jump-to-definition-key "C->")
     '(tss-implement-definition-key "C-c i"))
    (tss-config-default)))

;; Go
;(use-package go-mode :defer t)

;; FSharp
;(use-package fsharp-mode :defer t)

;; JSON
;(use-package json-mode :defer t)

;; YAML
;(use-package yaml-mode :defer t)

;; Markdown Mode
(use-package markdown-mode :defer t
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (progn
    (visual-line-mode nil)))

;(use-package 'realtime-preview :defer t)

;; Web
(defun my/web-mode-hook ()
  "Set variables for web-mode."
  (custom-set-variables
   '(web-mode-enable-auto-pairing nil)))

(defun sp-web-mode-is-code-context (id action context)
  "http://web-mode.org/"
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode :defer t
  :init
  (progn
    (add-hook 'web-mode-hook 'my/web-mode-hook)
    (--each '("\\.html\\'" "\\.tpl\\'" "\\.tpl\\.xhtml\\'" "\\.ejs\\'" "\\.hbs\\'")
      (add-to-list 'auto-mode-alist (cons it 'web-mode))))
  :config
  (progn
    (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))))

;; Emmet-mode
(use-package emmet-mode :defer t
  :init
  (progn
    (add-hook 'web-mode-hook  'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)))

;; pixiv Novel
;(use-package pixiv-novel-mode :defer t)

;;; Others:

;; Recentf
(use-package recentf-ext
  :init
  (progn
    (custom-set-variables
     '(recentf-max-saved-items 2000)
     '(recentf-auto-cleanup 100)
     (list 'recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list)))
    (recentf-mode t)
    (add-hook 'after-init-hook 'helm-recentf)
    (bind-key "C-c っ" 'helm-recentf)
    (bind-key "C-c t" 'helm-recentf)))

;; Undo Tree
(use-package undo-tree
  :init
  (global-undo-tree-mode))

;; expand-region.el
(use-package expand-region :defer t
  :init
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
  :init
  (progn
    (custom-set-variables
     '(open-junk-file-format "~/junk/%Y/%m/%Y-%m-%d-%H%M%S-"))
    (bind-key "C-c j" 'open-junk-file)))

;; restclient.el
(use-package restclient :defer t
  :mode ("\\.http\\'" . restclient-mode))

;; w3m
;(use-package w3m :defer t)

;; Org-IO Slide
(require 'ox-ioslide-helper)

;; navi2ch
(use-package navi2ch :defer t
  :config
  (progn
    (require 'navi2ch-mona)
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
  :init
  (progn
    (custom-set-variables
     '(elscreen-prefix-key (kbd "C-z"))
     '(elscreen-display-tab nil)
     '(elscreen-tab-display-kill-screen nil)
     '(elscreen-tab-display-control nil))
    ;(bind-key "C-t p" 'helm-elscreen)
    (bind-key "C-<tab>" 'elscreen-next)
    (bind-key "<C-iso-lefttab>" 'elscreen-previous)
    (elscreen-start)))

;; Calfw
(use-package calfw)
(use-package calfw-git)
(use-package calfw-syobocal
  :init
  (require 'syobo))

;; moccur
(use-package color-moccur)
;(use-package moccur-edit)

(use-package ag
  :init
  (progn
    (custom-set-variables
     '(ag-highlight-search t)
     '(ag-reuse-window 'nil)
     '(ag-reuse-buffers 'nil))
    (require 'wgrep-ag)
    (autoload 'wgrep-ag-setup "wgrep-ag")
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)
    (bind-key "C-:" 'ag))
  :config
  (progn
    (bind-key "r" 'wgrep-change-to-wgrep-mode ag-mode-map)))

;; direx
(use-package direx :defer t
  :init
  (progn
    (bind-key "M-C-\\" 'direx-project:jump-to-project-root-other-window)
    (bind-key "M-C-¥"  'direx-project:jump-to-project-root-other-window)))

;; dired-k
(use-package dired-k :defer t
  :init
  (progn
    (add-hook 'dired-initial-position-hook 'dired-k)
    (bind-key "K" 'dired-k dired-mode-map)))

;; Wdired
(use-package wdired)

;; Visual
(bind-key "M-%" 'vr/query-replace)

;; image-mode
(use-package image-mode :defer t
  :config
  (progn
    (bind-key "<wheel-up>"    'image-previous-line    image-mode-map)
    (bind-key "<wheel-down>"  'image-next-line        image-mode-map)
    (bind-key "<wheel-right>" 'image-forward-hscroll  image-mode-map)
    (bind-key "<wheel-left>"  'image-backward-hscroll image-mode-map)))

;; UCS Utility
;(use-package ucs-utils :defer t)

;; Font Utility
;(use-package font-utils)

;;; Games:
;(use-package gnugo :defer t)

;;; Communication:
(use-package twittering-mode :defer t
  :config
  (progn
    (custom-set-variables
     '(twittering-use-master-password t))
    (twittering+tern-on-yorufukurou)
    (twittering-icon-mode)))

;;; Server:
(use-package edit-server
  :if window-system
  :init
  (progn
    (add-hook 'after-init-hook 'server-start t)
    (add-hook 'after-init-hook 'edit-server-start t)))

;;; Variables:
(custom-set-variables
 '(ac-ignore-case nil)
 '(eldoc-minor-mode-string "")
 '(shr-max-image-proportion 2.5))

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;;; Functions:
(defvar my/hidden-minor-modes
  '(abbrev-mode
    auto-complete-mode
    eldoc-mode
    helm-mode
    paredit-mode
    magit-auto-revert-mode
    smart-newline-mode
    smartparens-mode
    undo-tree-mode
    yas-minor-mode))
(--each my/hidden-minor-modes
  (setq minor-mode-alist
        (cons (list it "") (assq-delete-all it minor-mode-alist))))

(defvar my/disable-trailing-modes
  '(cfw:calendar-mode
    comint-mode
    eshell-mode
    package-menu-mode
    eww-mode
    term-mode
    twittering-mode))
(--each my/disable-trailing-modes
  (add-hook (intern (concat (symbol-name it) "-hook"))
            'my/disable-trailing-mode-hook))

;;; My Functions:
(defun reload-major-mode ()
  "Reload current major mode."
  (interactive)
  (let ((current-mode major-mode))
    (fundamental-mode)
    (funcall current-mode)
    current-mode))

(defun toggle-load-theme ()
  "Toggle `load-theme'."
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    (load-theme
     (car (or (cdr (member current-theme my/load-themes))
              my/load-themes)))))

(defun my/disable-trailing-mode-hook ()
  "Disable show tail whitespace."
  (setq show-trailing-whitespace nil))

;; Original: http://qiita.com/ShingoFukuyama/items/e0be9497723b01905813
(defun my-outdent-dwim ()
  (interactive)
  (let* ((x-times (or current-prefix-arg 1))
         (mode-offset (if (boundp 'c-basic-offset) c-basic-offset 2))
         (offset (- (* mode-offset x-times))))
    (if mark-active
        (indent-rigidly
         (save-excursion (goto-char (region-beginning)) (point-at-bol))
         (save-excursion (goto-char (region-end)) (point-at-eol))
         offset)
      (indent-rigidly (point-at-bol) (point-at-eol) offset))))

(bind-key "<S-tab>" 'my-outdent-dwim)
;;; my-outdent-dwim ends here.

;; pick up after
(setq gc-cons-threshold (* 8 1024 1024))

;;; init.el ends here
