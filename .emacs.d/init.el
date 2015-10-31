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
(setq-default gc-cons-percentage 0.5)

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
(setq use-dialog-box nil)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(let ((default-directory (locate-user-emacs-file "./site-lisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; http://ergoemacs.org/emacs/emacs_n_unicode.html
;; set Unicode data file location. (used by what-cursor-position and describe-char)
(let ((file "~/emacs.d/UnicodeData.txt"))
  (when (file-exists-p file)
    (custom-set-variables
     (list 'describe-char-unicodedata-file file))))

;;; Font:
;;;     |いろはにほへと　ちりぬるを|
;;;     |わかよたれそ　　つねならむ|
;;;     |うゐのおくやま　けふこえて|
;;;     |あさきゆめみし　ゑひもせす|

(defvar my/default-display-font-family "Migu 2M")
(defvar my/display-font-size-by-hostname-alist
  '(("MegurineUbu1410"  . 12.5)
    ("tadsan-ret.local" . 17.5)))

(when (and window-system (>= emacs-major-version 23))
  (set-frame-font
   (format "%s-%.1f"
           my/default-display-font-family
           (or (cdr (assoc (system-name) my/display-font-size-by-hostname-alist))
               15.5))))

;;; Packages:
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(when (or (require 'cask "~/.cask/cask.el" t)
	  (require 'cask nil t))
  (cask-initialize))

(require 'use-package)
(pallet-mode t)

(defalias 'major-mode-of 'magic-filetype-major-mode-of)

(custom-set-variables '(nyan-bar-length 16))
(nyan-mode t)

;;; Environment:

;; PATH
(custom-set-variables
 '(exec-path-from-shell-variables '("PATH" "MANPATH" "GOROOT" "GOPATH")))
(exec-path-from-shell-initialize)

;;; Coding:
(setq-default indent-tabs-mode nil)

;; White space
(setq-default show-trailing-whitespace t)

;; Uniquify
(custom-set-variables '(uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Show paren
(show-paren-mode t)

;; Column mode
(column-number-mode t)

;; volatile-highlights.el
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :init
  (volatile-highlights-mode t))

;; Rainbow mode
(use-package rainbow-mode :defer t
  :diminish rainbow-mode)

;; Key config ;(use-package bind-key)
(progn
  (bind-key  "M-ESC ESC"   'keyboard-quit)
  (bind-key  "C-c R"       'revert-buffer)
  (bind-key  "C-c i"       'my/display-file-info)
  (bind-key  "C-x j"       'dired-jump)
  (bind-key  "C-x お"      'other-window)
  (bind-key  "M-："        'eval-expression)
  (bind-key  "M-ESC ："    'eval-expression)
  (bind-key  "<S-tab>"     'my/outdent-dwim)
  (bind-key  "C-M-y"       'helm-show-kill-ring)
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
    (when (boundp it) (set it 'meta)))))

;; key-chord
(use-package key-chord
  :init
  (custom-set-variables
   '(key-chord-two-keys-delay 0.02))
  (key-chord-mode t)
  (key-chord-define-global "df" 'find-function)
  (key-chord-define-global "fh" 'describe-function)
  (key-chord-define-global "@p" 'package-install)
  (key-chord-define-global "kl" 'align-regexp)
  (key-chord-define-global "rt" 'toggle-load-theme)
  (key-chord-define-global "wr" 'writeroom-mode)
  (key-chord-define-global "m," 'reload-major-mode))

(use-package sequential-command
  :config
  (define-sequential-command my/seq-home
    beginning-of-line beginning-of-line beginning-of-defun beginning-of-buffer seq-return)
  (define-sequential-command my/seq-end
    end-of-line end-of-line end-of-defun end-of-buffer seq-return)
  (bind-key "C-a" 'my/seq-home)
  (bind-key "C-e" 'my/seq-end))

;; Helm
(use-package helm :defer t
  :diminish helm-mode
  :config
  (progn
    (require 'helm-config)
    (helm-mode t)))

(use-package helm-ag :defer t
  :init
  (progn
    (bind-key "C-:" 'helm-ag)))

;; Auto-Complete
(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (add-to-list 'ac-dictionary-directories (locate-user-emacs-file "./ac-dict"))
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t))

;; Magit
(use-package magit :defer t
  :init
  (setq-default magit-auto-revert-mode nil)
  (setq vc-handled-backends '())
  (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
  (bind-key "C-x m" 'magit-status)
  (bind-key "C-c l" 'magit-blame))
(use-package magit-find-file :defer t
  :init
  (bind-key "M-t" 'magit-find-file-completing-read))

(add-to-list 'auto-mode-alist '("/\\.gitexclude\\'" . gitignore-mode))


;; Conf-Mode
(require 'generic-x)
(add-to-list 'auto-mode-alist '("/\\.*config\\'"  . conf-mode) t)
(add-to-list 'auto-mode-alist '("/\\.*rc\\'"  . conf-mode) t)

(use-package ssh-config-mode)

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
 :diminish flycheck-mode)
(global-flycheck-mode t)

;; Smartparens
(use-package smartparens
  :diminish smartparens-mode)
(require 'smartparens-config)
(smartparens-global-mode t)

;; which-func
(which-function-mode t)

;; smartchr
(use-package smartchr :defer t
  :commands smartchr)

;; YASnippets
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode t))

;;; Languages:

(custom-set-variables
 '(sql-product 'mysql))

;; Web
(defun my/web-mode-hook ()
  "Set variables for web-mode."
  (custom-set-variables
   '(web-mode-enable-auto-pairing nil)))

(defun sp-web-mode-is-code-context (id action context)
  "This snippet is derived from http://web-mode.org/ ."
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode :defer t
  :init
  (progn
    (add-hook 'web-mode-hook 'my/web-mode-hook)
    (add-hook 'web-mode-hook 'emmet-mode)
    (--each '("\\.html?\\'" "\\.tpl\\'" "\\.tpl\\.xhtml\\'" "\\.ejs\\'" "\\.hbs\\'" "\\.jsx\\'")
      (add-to-list 'auto-mode-alist (cons it 'web-mode))))
  :config
  (progn
    (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))))

(add-to-list 'auto-mode-alist '("/Gemfile.lock\\'" . conf-mode))


;; PHP
(use-package php-mode :defer t
  :config
  (progn
    (require 'php-extras)
    ;(php-extras-eldoc-documentation-function)
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
(add-to-list 'auto-mode-alist `("/composer.lock\\'" . ,(major-mode-of 'json)))

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
;(use-package rhtml-mode :defer t)

;; inf-ruby
(use-package inf-ruby :defer t
  :config
  (custom-set-variables
   '(inf-ruby-default-implementation "pry")
   '(inf-ruby-eval-binding "Pry.toplevel_binding"))
  (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on))

;; Python
(use-package python :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; Lisp
(defun my/emacs-lisp-mode-hook ()
  ""
  (rainbow-mode t)
  (set-face-foreground 'font-lock-regexp-grouping-backslash "indian red")
  (set-face-foreground 'font-lock-regexp-grouping-construct "peru"))

(defvar my/emacs-lisp-modes
  '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook))
(--each my/emacs-lisp-modes
  (add-hook it 'turn-on-eldoc-mode)
  (add-hook it 'my/emacs-lisp-mode-hook))

;; `Cask' is NOT emacs-lisp-mode
(add-to-list 'auto-mode-alist '("/Cask\\'" . lisp-mode))

(use-package paredit :defer t
  :diminish paredit-mode
  :init
  (--each my/emacs-lisp-modes (add-hook it 'enable-paredit-mode))
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


;; Haskell
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

;; CoffeeScript
(use-package coffee :defer t
  :config
  (progn
    (setq-default coffee-tab-width 2)
    (defun my/coffee-hook ()
      (set (make-local-variable 'tab-width) 2))
    (add-hook 'coffee-mode 'my/coffee-hook)))

;; Facebook JSX
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  "This snippet is derived from https://truongtx.me/2014/03/10/emacs-setup-jsx-mode-and-jsx-syntax-checking/ ."
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (add-to-list 'web-mode-comment-formats '("jsx" . "// " ))
              (flycheck-mode))))

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

(when nil
  (flycheck-define-checker my/json-lint
  "JSON Syntax check using Python json"
  :command ("python" "-mjson.tool" source)
  :error-patterns
  ((error line-start "No JSON object could be decoded" line-end)
   ;; [Python 2] Expecting object: line 1 column 2 (char 1)
   ;; [Python 3] Expecting property name enclosed in double quotes: line 2 column 5 (char 6)
   (error line-start (message) ": line " line " column " column " (char " (one-or-more char) ")" line-end)
   ;; Extra data: line 1 column 43 - line 3 column 1 (char 42 - 86)
   )
  :modes 'json-mode))

;; YAML
;(use-package yaml-mode :defer t)

;; Markdown Mode
(use-package markdown-mode :defer t
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (progn
    ;(unbind-key "`" markdown-mode-map)
    (visual-line-mode nil)))

;(use-package 'realtime-preview :defer t)

;; Emmet-mode
(use-package emmet-mode :defer t
  :init
  (progn
    (add-hook 'web-mode-hook  'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)))

;; pixiv Novel
;(use-package pixiv-novel-mode :defer t)

;; Magic Filetype
;(use-package magic-filetype)
(magic-filetype-enable-vim-filetype)

;;; Others:

;; Recentf
(use-package recentf-ext
  :init
  (custom-set-variables
   '(recentf-max-saved-items 2000)
   '(recentf-auto-cleanup 100)
   '(recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores"))
   (list 'recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list)))
  (recentf-mode t)
  (bind-key "C-c っ" 'helm-recentf)
  (bind-key "C-c t" 'helm-recentf))

;; Undo Tree
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  (bind-key "C-_" #'undo-tree-undo)
  (bind-key "C-?" #'undo-tree-redo))

;; expand-region.el
(use-package expand-region :defer t
  :init
  (bind-key "C-@" 'er/expand-region)
  (bind-key "C-`" 'er/contract-region))

;; Annotate.el
(use-package annotate :defer t
  :init
  (bind-key "M-@"   'annotate-annotate)
  (bind-key "C-M-@" 'annotate-clear-annotations))

;;; Tools:

;; term+
(use-package term+
  :config
  (use-package term+key-intercept)
  (use-package term+mux)
  (require 'xterm-256color))

;; Open junk file
(use-package open-junk-file
  :init
  (custom-set-variables
   '(open-junk-file-format "~/junk/%Y/%m/%Y-%m-%d-%H%M%S-"))
  (bind-key "C-c j" 'open-junk-file))

;; restclient.el
(use-package restclient :defer t
  :mode ("\\.http\\'" . restclient-mode))

;; w3m
;(use-package w3m :defer t)

(use-package org-mode :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

;; Org-IO Slide
(require 'ox-ioslide-helper)

;; navi2ch
(use-package navi2ch :defer t
  :config
  (require 'navi2ch-mona)
  (custom-set-variables
   '(navi2ch-article-use-jit t)
   '(navi2ch-article-exist-message-range nil)
   '(navi2ch-article-new-message-range nil)
   '(navi2ch-mona-enable t)
   '(navi2ch-mona-use-ipa-mona t)
   '(navi2ch-mona-ipa-mona-font-family-name "mona-izmg16"))
  (navi2ch-mona-setup))

;; EMMS http://www.emacswiki.org/emacs/EMMS
(use-package emms :defer t
  :config
  (require 'emms)
  (use-package emms-player-mpv)
  (add-to-list 'emms-player-list 'emms-player-mpv)
  (use-package emms-player-mpv-jp-radios)
  (let ((default-directory (file-name-directory (find-lisp-object-file-name 'emms-player-mpv-jp-radios-add 'defun))))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path))
  (emms-player-mpv-jp-radios-add-all))

;; ElScreen
(use-package elscreen
  :init
  (custom-set-variables
   '(elscreen-prefix-key (kbd "C-z"))
   '(elscreen-display-tab nil)
   '(elscreen-tab-display-kill-screen nil)
   '(elscreen-tab-display-control nil))
  ;;(bind-key "C-t p" 'helm-elscreen)
  (bind-key "C-<tab>" 'elscreen-next)
  (bind-key "<C-iso-lefttab>" 'elscreen-previous)
  (elscreen-start))

;; Calfw
;; (use-package calfw)
;; (use-package calfw-git)
;; (use-package calfw-syobocal
;;   :init
;;   (require 'syobo))

;; moccur
(use-package color-moccur)
;(use-package moccur-edit)

(use-package ag
  :init
  (custom-set-variables
   '(ag-highlight-search t)
   '(ag-reuse-window 'nil)
   '(ag-reuse-buffers 'nil))
  (require 'wgrep-ag)
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  (bind-key "M-C-:" 'ag)
  :config
  (bind-key "r" 'wgrep-change-to-wgrep-mode ag-mode-map))

;; Swoop
(use-package helm-swoop
  :init
  (bind-key "C-;" 'helm-swoop)
  (bind-key "M-C-;" 'helm-multi-swoop))

;; direx
(use-package direx :defer t
  :init
  (bind-key "M-C-\\" 'direx-project:jump-to-project-root-other-window)
  (bind-key "M-C-¥"  'direx-project:jump-to-project-root-other-window))

;; dired-k
(use-package dired-k :defer t
  :init
  (add-hook 'dired-initial-position-hook 'dired-k)
  (bind-key "K" 'dired-k dired-mode-map))

;; Wdired
(use-package wdired)

;; Visual
(bind-key "M-%" 'vr/query-replace)

;; image-mode
(use-package image-mode :defer t
  :config
  (bind-key "<wheel-up>"    'image-previous-line    image-mode-map)
  (bind-key "<wheel-down>"  'image-next-line        image-mode-map)
  (bind-key "<wheel-right>" 'image-forward-hscroll  image-mode-map)
  (bind-key "<wheel-left>"  'image-backward-hscroll image-mode-map))

;; multiple-cursors
;; http://qiita.com/ongaeshi/items/3521b814aa4bf162181d
(use-package multiple-cursors
  :init
  (require 'smartrep)
  (declare-function smartrep-define-key "smartrep")
  (bind-key "C-M-c" 'mc/edit-lines)
  (bind-key "C-M-r" 'mc/mark-all-in-region)
  (global-unset-key (kbd "C-t"))
  (smartrep-define-key global-map "C-t"
    '(("C-t" . 'mc/mark-next-like-this)
      ("n"   . 'mc/mark-next-like-this)
      ("p"   . 'mc/mark-previous-like-this)
      ("m"   . 'mc/mark-more-like-this-extended)
      ("u"   . 'mc/unmark-next-like-this)
      ("U"   . 'mc/unmark-previous-like-this)
      ("s"   . 'mc/skip-to-next-like-this)
      ("S"   . 'mc/skip-to-previous-like-this)
      ("*"   . 'mc/mark-all-like-this)
      ("d"   . 'mc/mark-all-like-this-dwim)
      ("i"   . 'mc/insert-numbers)
      ("o"   . 'mc/sort-regions)
      ("O"   . 'mc/reverse-regions))))

;; smooth-scroll https://github.com/k-talo/smooth-scroll.el
(use-package smooth-scroll
  :diminish smooth-scroll-mode
  :init
  (require 'smooth-scroll)
  (custom-set-variables
   '(smooth-scroll/vscroll-step-size 7))
  (smooth-scroll-mode t))

;; UCS Utility
;(use-package ucs-utils :defer t)

;; Font Utility
;(use-package font-utils)

(use-package emoji-fontset
  :init
  (emoji-fontset/turn-on "Symbola"))

;; TRAMP
(use-package tramp :defer t
  :config
  (require 'vagrant-tramp)
  (vagrant-tramp-enable)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;; Games:
;(use-package gnugo :defer t)

;;; Communication:
(use-package twindrill-mode :defer t
  :config
  (custom-set-variables
   '(twindrill-use-master-password t))
  (twindrill+tern-on-yorufukurou))

;;; Variables:
(custom-set-variables
 '(ac-ignore-case nil)
 '(eldoc-minor-mode-string "")
 '(shr-max-image-proportion 2.5))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Functions:
(defmacro safe-diminish (file mode &optional new-name)
  "
https://github.com/larstvei/dot-emacs/blob/master/init.org"
  `(with-eval-after-load ,file
     (diminish ,mode ,new-name)))

(safe-diminish "abbrev" 'abbrev-mode)
(safe-diminish "eldoc" 'eldoc-mode)
(safe-diminish "flyspell" 'flyspell-mode)
(safe-diminish "simple" 'auto-fill-function)

(defvar my/disable-trailing-modes
  '(Buffer-menu-mode
    calendar-mode
    cfw:calendar-mode
    comint-mode
    eshell-mode
    package-menu-mode
    eww-mode
    term-mode))
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

(defun find-file-as-sudo (filename)
  "Find `FILENAME' as root."
  (interactive "FOpen: ")
  (find-file (concat "/sudo::" (replace-regexp-in-string "^sudo:[^:]*:" "" filename))))

(defun my/disable-trailing-mode-hook ()
  "Disable show tail whitespace."
  (setq show-trailing-whitespace nil))

;; Original: http://qiita.com/ShingoFukuyama/items/e0be9497723b01905813
(defun my/outdent-dwim ()
  "Outdent!"
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
;; my/outdent-dwim ends here

;; Original: http://ja.stackoverflow.com/questions/12510
(defun my/insert-kbd-sequence ()
  "Insert (kbd) sequence."
  (interactive)
  (insert (concat "(kbd \""
                  (key-description (read-key-sequence "input> "))
                  "\")")))
;; my/insert-kbd-sequence ends here

(init-open-recentf)

(elscreen-create)

;;; init.el ends here
