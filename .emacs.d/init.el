;;; init.el --- The zonuexe's .emacs -*- coding: utf-8 ; lexical-binding: t -*-

;; Filename: init.el
;; Description: zonuexe's .emacs
;; Package-Requires: ((emacs "26.1"))
;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 2014-11-01
;; Modified: 2018-09-03
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
;; Nobiscum Sexp.  - S-expression is with us.
;;
;;; Code:
(if window-system
    (tool-bar-mode -1)
  (menu-bar-mode -1))

;;; Color-theme:
(custom-set-variables
 '(custom-safe-themes
   '("7f968c172d6ec46766773a8304c7570bdff45f1220d3700008a437d9529ca3e4"
     "89fc84ffb9681d9bf8c05a5642dff5f1078fd8b892e974bcfd400f17929cdead"
     "b4117b5e16a0d1d9a265cbdc0c4062f4b3f832da38316f9d65ea39f1b2dd0063"
     default)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-doc-face ((t (:slant normal)))))

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
(load (locate-user-emacs-file "./site-lisp/site-lisp-autoloads.el") t)

;;; Font:
;;;     |いろはにほへと　ちりぬるを|
;;;     |わかよたれそ　　つねならむ|
;;;     |うゐのおくやま　けふこえて|
;;;     |あさきゆめみし　ゑひもせす|

(defvar my/font-family "Migu 2M")
(defvar my/font-size
  (let ((size-by-hostname
         '(("MegurineUbu1410"  . 12.5)
           ("MegurineUbu1510"  . 12.5)
           ("MegurineUbu1604"  . 12.5)
           ("Megurine-Manjaro" . 14.0)
           ("tadsan-tpx"       . 12.5)
           ("tadsan-ret.local" . 16.5))))
    (or (cdr (assoc (system-name) size-by-hostname))
        15.5)))

(when window-system
  ;; http://d.hatena.ne.jp/kitokitoki/20110502/p2
  (let ((fontset (format "%s-%.1f" my/font-family my/font-size)))
    (add-to-list 'default-frame-alist `(font . ,fontset)))
  (add-to-list 'default-frame-alist `(cursor-type . (hbar . ,(1+ (ceiling (/ my/font-size 2)))))))

;; Set and load custom-vars.el
(setq custom-file (expand-file-name "custom-vars.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

;;; Packages:
(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://www.mirrorservice.org/sites/melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (version< emacs-version "27")
  (package-initialize))

(require 'dash)

;;; Environment:

;; PATH
(custom-set-variables
 '(exec-path-from-shell-check-startup-files nil)
 '(exec-path-from-shell-variables '("PATH" "TEST_SERVER" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "MANPATH" "GOROOT" "GOPATH" "PERL5LIB")))

(unless (eq window-system 'nt)
  (exec-path-from-shell-initialize))

;; (when (eq window-system 'w32)
;;   (setenv "GIT_SSH" "C:\\Program Files\\PuTTY\\plink.exe"))

(when (file-directory-p "~/repo/emacs/php-mode")
  (load "~/repo/emacs/php-mode/php-mode-autoloads.el"))

(when (file-directory-p "~/repo/emacs/auto-complete")
  (load "~/repo/emacs/auto-complete/auto-complete-autoloads.el"))

(when (file-directory-p "~/repo/emacs/phpactor.el")
  (load "~/repo/emacs/phpactor.el/phpactor-autoloads.el"))

(when (file-directory-p "~/repo/emacs/emacs-phps-mode")
  (load "~/repo/emacs/emacs-phps-mode/phps-mode-autoloads.el"))

(when (file-directory-p "~/repo/emacs/sunrise-commander")
  (load "~/repo/emacs/sunrise-commander/sunrise-commander-autoloads.el"))

;; load private config
(require 'my/private "my-private.el" t)


;; benchmark-init
;; https://github.com/dholm/benchmark-init-el
(benchmark-init/activate)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(defalias 'major-mode-of 'magic-filetype-major-mode-of)

(use-package nyan-mode :defer t
  :custom
  (nyan-bar-length 16)
  :init
  (nyan-mode t))

;;; Coding:
(setq-default indent-tabs-mode nil)

;; White space
(setq-default show-trailing-whitespace t)

;; Uniquify
(custom-set-variables '(uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Show paren
(use-package paren
  :init
  (show-paren-mode t))

;; Column mode
(use-package simple
  :init
  (column-number-mode t))

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
  (bind-key  "C-S-n"       'make-frame)
  (bind-key  "C-S-w"       'delete-frame)
  (bind-key  "M-N"         'untitled-new-buffer)
  (bind-key  "C-M-S-d"     'projectile-dired)
  (bind-key  "C-c :"       'right-click-context-menu)
  (bind-key  "C-c ;"       'imenu)
  (bind-key  "C-c R"       'revert-buffer)
  (bind-key  "C-c S-i"     'my/display-file-info)
  (bind-key  "C-x j"       'dired-jump)
  (bind-key  "C-x C-S-e"   'pp-eval-last-sexp)
  (bind-key  "C-x お"      'other-window)
  (bind-key  "C-S-v"       'scroll-down-command)
  (bind-key  "M-o"         'swoop)
  (bind-key  "C-M-o"       'swoop-multi)
  (bind-key  "M-："        'eval-expression)
  (bind-key  "M-i"         'helm-imenu prog-mode-map)
  (bind-key  "M-ESC ："    'eval-expression)
  (bind-key  "<S-tab>"     'my/outdent-dwim)
  (bind-key  "C-M-y"       'helm-show-kill-ring)
  (bind-key  "M-<left>"    'bs-cycle-previous)
  (bind-key  "M-<right>"   'bs-cycle-next)
  (bind-key  "C-M-S-y"     'my/kill-buffer-file-name)
  (bind-key  "M-<f5>"      'compile)
  (bind-key  "<f5>"        'quickrun)
  (bind-key  "<f9>"        'zone)
  (bind-key  "<f12>"       'neotree-toggle)
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
(use-package key-chord :defer t
  :custom
  (key-chord-two-keys-delay 0.02)
  :init
  (key-chord-mode 1)
  :config
  (key-chord-define-global "df" 'find-function)
  (key-chord-define-global "fh" 'describe-function)
  (key-chord-define-global "fv" 'find-variable)
  (key-chord-define-global "jb" 'jetbrains-open-buffer-file)
  (key-chord-define-global "@p" 'package-install)
  (key-chord-define-global "kl" 'align-regexp)
  (key-chord-define-global "rt" 'toggle-load-theme)
  (key-chord-define-global "wr" 'writeroom-mode)
  (key-chord-define-global "m," 'reload-major-mode)
  (key-chord-define-global "mc" 'my/buffer-minchoize))

;; Helm
(use-package helm :defer t
  :diminish helm-mode
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-smex)
         ("M-X" . helm-smex-major-mode-commands)
         ("C-:" . helm-ag-project-root))
  :init
  (require 'helm-config)
  (helm-mode t))

(use-package helm-ag :defer t
  :custom
  (helm-ag-base-command "rg --vimgrep --no-heading")
  (helm-ff-file-compressed-list '("epub" "gz" "bz2" "zip" "7z")))

;; ispell
(use-package ispell
  :custom
  (ispell-program-name "hunspell")
  (ispell-really-hunspell t))

(use-package eldoc :defer t
  :diminish eldoc-mode
  :custom
  (eldoc-minor-mode-string ""))

;; Auto-Complete
(use-package auto-complete :defer t
  :diminish auto-complete-mode
  :custom
  (ac-ignore-case nil)
  :config
  (add-to-list 'ac-dictionary-directories (locate-user-emacs-file "./ac-dict"))
  (require 'auto-complete-config)
  (ac-config-default)
  ;;(ac-ispell-setup)
  (global-auto-complete-mode t))

;; Magit
(use-package magit :defer t
  :bind (("C-x m" . magit-status)
         ("C-c l" . magit-blame-addition))
  :init
  (setq-default magit-auto-revert-mode nil)
  (setq vc-handled-backends '())
  (eval-after-load "vc" '(remove-hook 'find-file-hook 'vc-find-file-hook)))

(use-package magit-find-file :defer t
  :bind (("M-t" . magit-find-file-completing-read)))

(use-package gitignore-mode :defer t
  :mode ("/\\.gitexclude\\'" "/\\.\\(?:ag\\|docker\\)?ignore\\'"))

;; EditorConfig
(use-package editorconfig :defer t
  :diminish editorconfig-mode
  :custom
  (editorconfig-get-properties-function 'editorconfig-core-get-properties-hash)
  :init
  (editorconfig-mode t))

;; Conf-Mode
(use-package conf-mode
  :init
  (require 'generic-x)
  (add-to-list 'auto-mode-alist '("/\\.env\\(?:\\.sample\\)?\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("/\\.*conf\\(?:ig\\)?\\'" . conf-mode) t)
  (add-to-list 'auto-mode-alist '("/\\.*rc\\'" . conf-mode) t))

;; SSH
;;(use-package ssh-config-mode)

;; Projectile
(use-package projectile :defer t
  :hook ((projectile-mode . projectile-rails-on))
  :custom
  (projectile-enable-caching nil)
  (projectile-completion-system 'helm))

(use-package helm-projectile :defer t
  :config
  (helm-projectile-on))

;; Flycheck
(use-package flycheck :defer t
  :diminish flycheck-mode
  :hook ((flycheck-mode . flycheck-cask-setup))
  :init
  (global-flycheck-mode t)
  (flycheck-package-setup))

;; elec-pair
(use-package elec-pair :defer t
  :init
  (electric-pair-mode 1))

;; which-func
(use-package which-func :defer t
  :init
  (which-function-mode 1))

;; smartchr
(use-package smartchr :defer t
  :commands smartchr)

;; YASnippets
(use-package yasnippet
  :diminish yas-minor-mode
  :custom
  (yas-alias-to-yas/prefix-p nil)
  :init
  (yas-global-mode t))

(defun my-presentation-on ()
  (helm-mode -1)
  (ido-ubiquitous-mode 1)
  (bind-key "M-x" #'smex))

(defun my-presentation-off ()
  (ido-ubiquitous-mode -1)
  (helm-mode 1)
  (bind-key "M-x" #'helm-smex))

(use-package presentation-mode :defer t
  :hook ((presentation-on  . my-presentation-on)
         (presentation-off . my-presentation-off)))

;;; Languages:
(use-package sql :defer t
  :custom
  (sql-product 'mysql))

;; Web
(defun my-web-mode-setup ()
  "Set variables for web-mode."
  (emmet-mode +1)
  (when (and buffer-file-name (string= "tsx" (file-name-extension buffer-file-name)))
    (my-setup-typescript)))

(defun sp-web-mode-is-code-context (_id action _context)
  "This snippet is derived from http://web-mode.org/ ."
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode :defer t
  :hook ((web-mode . my-web-mode-setup))
  :mode
  ("\\.html?\\'" "\\.tpl\\'" "\\.tpl\\.xhtml\\'" "\\.ejs\\'" "\\.hbs\\'"
   "\\(\\.html\\)?\\.erb\\'" "\\.tsx\\'" "\\.vue\\'")
  :custom
  (web-mode-enable-auto-pairing nil)
  (web-mode-enable-auto-indentation nil)
  :config
  (require 'smartparens)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-to-list 'web-mode-ac-sources-alist
               '("html" . (ac-source-html-tag ac-source-html-attr ac-source-html-attrv)))
  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context)))

;; PHP
(defun my/turn-on-php-eldoc ()
  "Turn on php-eldoc."
  (when (require 'php-eldoc nil t)
    (php-eldoc-enable)))

(defun my-php-mode-setup ()
  "My PHP-mode hook."
  ;;(require 'company-phpactor)
  (my/turn-on-php-eldoc)
  (subword-mode 1)
  (setq show-trailing-whitespace t)

  (setq-local ac-disable-faces '(font-lock-comment-face font-lock-string-face))

  (require 'flycheck-phpstan)
  (flycheck-mode t)
  (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
  (add-to-list 'flycheck-disabled-checkers 'php-phpcs)

  (when (and buffer-file-name (string-match-p "/pixiv/" buffer-file-name))
    (require 'pixiv-dev nil t)
    (pixiv-dev-mode t))

  (when (eq 0 (buffer-size))
    (insert "<?php\n\n")))

(add-to-list 'auto-minor-mode-alist '("/pixiv/" . pixiv-dev-mode))

(use-package php-mode
  :hook ((php-mode . my-php-mode-setup))
  :custom
  (php-default-major-mode 'php-mode)
  (php-manual-url 'ja)
  (php-mode-coding-style 'psr2)
  (php-mode-template-compatibility nil)
  :config
  ;;(require 'php-extras)
  ;;(php-extras-eldoc-documentation-function)
  ;;(require 'ac-php)
  ;;(setq ac-php-use-cscope-flag  t ) ;;enable cscope

  (bind-key "[" (smartchr "[]" "array()" "[[]]") php-mode-map)
  (bind-key "]" (smartchr "array " "]" "]]")     php-mode-map)
  ;; (bind-key "C-}" 'cedit-barf php-mode-map)
  ;; (bind-key "C-)" 'cedit-slurp php-mode-map)
  (bind-key "C-c C-c" 'psysh-eval-region         php-mode-map)
  (bind-key "<f6>" 'phpunit-current-project      php-mode-map)
  (bind-key "C-c C--" 'php-current-class php-mode-map)
  (bind-key "C-c C-=" 'php-current-namespace php-mode-map))

(use-package psysh
  :custom
  (psysh-doc-display-function #'popwin:display-buffer))

(add-to-list 'auto-mode-alist `("/composer.lock\\'" . ,(major-mode-of 'json)))

(use-package psysh :defer t
  :hook ((psysh-mode . my/turn-on-php-eldoc)))

(use-package pixiv-dev :defer t
  :custom
  (pixiv-dev-user-name "tadsan")
  :init
  (autoload 'pixiv-dev-shell "pixiv-dev" nil t)
  (autoload 'pixiv-dev-find-file "pixiv-dev" nil t)
  (autoload 'pixiv-dev-copy-file-url "pixiv-dev" nil t))

(use-package phan :defer t
  :mode (("/\\(phan\\|filter\\)\\(?:-.+\\)?\\.log\\'" . phan-log-mode))
  :config
  (when (fboundp 'phan-flycheck-setup)
    (phan-flycheck-setup)))

;; Ruby
(defun my-enh-ruby-mode-setup ()
  "Setup function for `enh-ruby-mode'."
  (setq-local ac-ignore-case t))

(use-package enh-ruby-mode :defer t
  :mode (("\\.rb\\'" . enh-ruby-mode))
  :hook ((enh-ruby-mode . my-enh-ruby-mode-setup))
  :interpreter "pry"
  :config
  (subword-mode t)
  (yard-mode t)
  (add-to-list 'ac-modes 'enh-ruby-mode)
  (custom-set-variables
   '(ruby-deep-indent-paren-style nil))
  (setq-default enh-ruby-not-insert-magic-comment t))

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

;; inf-ruby
(use-package inf-ruby :defer t
  :hook ((inf-ruby-mode . ansi-color-for-comint-mode-on))
  :custom
  (inf-ruby-default-implementation "pry")
  (inf-ruby-eval-binding "Pry.toplevel_binding"))

;; Python
(use-package python :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; Lisp
(defvar my/emacs-lisp-ac-sources
  '(ac-source-features ac-source-functions ac-source-variables ac-source-symbols))

(defun my-emacs-lisp-mode-setup ()
  "Setup function for Emacs Lisp."
  (rainbow-mode t)
  (auto-complete-mode 1)
  (setq ac-sources (append ac-sources my/emacs-lisp-ac-sources))
  (set-face-foreground 'font-lock-regexp-grouping-backslash "indian red")
  (set-face-foreground 'font-lock-regexp-grouping-construct "peru")
  (nameless-mode t)
  (turn-on-eldoc-mode)
  (elisp-slime-nav-mode +1))

(use-package nameless :defer t
  :diminish nameless-mode
  :config
  (add-to-list 'nameless-global-aliases '("pv" . "projectile-variable")))

(defvar my/emacs-lisp-modes
  '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook))
(--each my/emacs-lisp-modes
  (add-hook it #'my-emacs-lisp-mode-setup))

(add-hook 'lisp-interaction-mode-hook #'turn-on-orgtbl)

(defalias 'inferior-emacs-lisp 'ielm "λ...")

;; `Cask' is NOT emacs-lisp-mode
(add-to-list 'auto-mode-alist '("/Cask\\'" . lisp-mode))

(use-package lsp-mode :defer t
  :hook ((lsp-after-open . lsp-enable-imenu)))

(use-package paredit :defer t
  :diminish paredit-mode
  :bind (:map paredit-mode-map
         ("C-<right>" . right-word)
         ("C-<left>"  . left-word))
  :init
  (--each my/emacs-lisp-modes (add-hook it 'enable-paredit-mode)))

;; Scheme
(defun my-scheme-mode-setup ()
  "λ..."
  (paredit-mode t)
  (ac-geiser-setup))

(use-package scheme :defer t
  :hook ((geiser-mode-hook . my/scheme-mode-hook)
         (scheme-mode-hook . my/scheme-mode-hook))
  :custom
  (geiser-active-implementations '(guile racket)))

;; Common Lisp
;; (use-package sly :defer t
;;   :init
;;   (require 'sly-autoloads)
;;   (custom-set-variables
;;    '(inferior-lisp-program "sbcl")))

;; Haskell
(use-package haskell-mode :defer t
  :hook ((haskell-mode . turn-on-eldoc-mode)
         (haskell-mode . turn-on-haskell-indent)))

;; JavaScript
(use-package js2-mode :defer t
  :mode ("\\.js\\'"))

(use-package rjsx-mode :defer t
  :mode ("\\.jsx\\'"))

;; CoffeeScript
(use-package coffee :defer t
  :config
  (setq-default coffee-tab-width 2))

;; TypeScript
(defun my-setup-typescript ()
  "Setup function for TypeScript."
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(use-package typescript :defer t
  :mode ("\\.ts\\'" . typescript-mode)
  :hook ((typescript-mode . my-setup-typescript)))

;; Go
;;(use-package go-mode :defer t)

;; FSharp
;;(use-package fsharp-mode :defer t)

;; JSON
;;(use-package json-mode :defer t)

;; text-mode
(defun my-text-mode-setup ()
  "Setup function for `text-mode'."
  (unless (eq major-mode 'html-mode)
    (setq line-spacing 5)))

(use-package text-mode :defer t
  :mode ("/LICENSE\\'")
  :hook ((text-mode . my-text-mode-setup)))

;; YAML
(use-package yaml-mode :defer t
  :mode ("/\\.gemrc\\'"))

;; Markdown Mode
(use-package markdown-mode :defer t
  ;;:mode ("\\.md\\'" . commonmark-gfm-mode)
  :config
  (require 'org-table)
  (add-hook 'markdown-mode-hook 'orgtbl-mode)
  (unbind-key "`" gfm-mode-map)
  (visual-line-mode nil))

;;(use-package 'realtime-preview :defer t)

;; Emmet-mode
(use-package emmet-mode :defer t
  :hook (web-mode-hook css-mode))

;; pixiv Novel
;;(use-package pixiv-novel-mode :defer t)

;; Magic Filetype
(use-package magic-filetype :defer t
  :init
  (magic-filetype-set-auto-mode 'ruby)
  (magic-filetype-enable-vim-filetype))

;;; Others:

;; Save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Recentf
(use-package recentf-ext
  :bind (("C-c っ" . helm-recentf)
         ("C-c t"  . helm-recentf))
  :custom
  (recentf-max-saved-items 2000)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.cache/"
                     "/\\.emacs\\.d/\\.cask/" "/\\newsrc\\(\\.eld\\)?\\'" "/elpa/.*-autoloads\\.el\\'"))
  (recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  :init
  (recentf-mode t))

;; Undo Tree
(use-package undo-tree :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  (bind-key "C-_" #'undo-tree-undo)
  (bind-key "C-?" #'undo-tree-redo))

;; expand-region.el
(use-package expand-region :defer t
  :bind (("C-@" . er/expand-region)
         ("C-`" . er/contract-region)))

;; Annotate.el
(use-package annotate :defer t
  :bind (("M-@"   . annotate-annotate)
         ("C-M-@" . annotate-clear-annotations)))

;;; Tools:

;; Open junk file
(use-package open-junk-file
  :bind (("C-c j" . open-junk-file))
  :custom
  (open-junk-file-format "~/junk/%Y/%m/%Y-%m-%d-%H%M%S-"))

;; restclient.el
(use-package restclient :defer t
  :mode ("\\.http\\'" . restclient-mode))

;; w3m
;;(use-package w3m :defer t)

(use-package org-mode :defer t
  :init
  (custom-set-variables
   '(org-default-notes-file (concat org-directory "/capture.org")))
  (bind-key "C-c c" 'org-capture)
  (autoload 'ioslide:helper "ox-ioslide-helper.el" "Key menu for ioslide" t)
  :config
  (org-ac/config-default)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

;; Org-IO Slide
;;(require 'ox-ioslide-helper)

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
   '(navi2ch-mona-face-variable 'navi2ch-mona16-face)
   '(navi2ch-mona-ipa-mona-font-family-name "mona-izmg16"))
  (navi2ch-mona-setup))

;; EMMS http://www.emacswiki.org/emacs/EMMS
(use-package emms :defer t
  :config
  (require 'emms)
  (use-package emms-player-mpv)
  (add-to-list 'emms-player-list 'emms-player-mpv)
  (use-package emms-player-mpv-jp-radios)
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
  (bind-key* "C-<tab>" 'elscreen-next)
  (bind-key* "<C-iso-lefttab>" 'elscreen-previous)
  (elscreen-start)
  ;; El-Screeのウィンドウを一個つくる
  (elscreen-create))

;; Calfw
;; (use-package calfw)
;; (use-package calfw-git)
;; (use-package calfw-syobocal
;;   :init
;;   (require 'syobo))

;; moccur
;;(use-package color-moccur)
;;(use-package moccur-edit)

(use-package rg :defer t
  :bind (("C-:" . rg)
         ("M-C-:" . rg-literal))
  :hook ((rg-mode . wgrep-rg-setup)))

;; Swoop
(use-package helm-swoop
  :bind (("C-;" . helm-swoop)
         ("M-C-;" . helm-multi-swoop)))

;; direx
(use-package direx :defer t
  :bind (("M-C-\\" . direx-project:jump-to-project-root-other-window)
         ("M-C-¥"  . direx-project:jump-to-project-root-other-window)))

;; dired-k
(use-package dired-k :defer t
  :bind ((:map dired-mode-map
          ("K" . dired-k)))
  :hook ((dired-initial-position-hook . dired-k)))

;; Wdired
;; (use-package wdired :defer t)

;; Visual
(use-package visual-regexp :defer t
  :bind (("M-%" . vr/query-replace)))

;; image-mode
(use-package image-mode :defer t
  :bind (:map image-mode-map
         ("<wheel-up>"    . image-previous-line)
         ("<wheel-down>"  . image-next-line)
         ("<wheel-right>" . image-forward-hscroll)
         ("<wheel-left>"  . image-backward-hscroll)))

;; Yet another folding
(use-package yafolding :defer t
  :hook ((prog-mode . yafolding-mode)))

;; NeoTree
(defun my/neotree-kill-filename-at-point ()
  "Kill full path of note at point."
  (interactive)
  (message "Copy %s"
           (kill-new (neo-buffer--get-filename-current-line))))

(use-package neotree :defer t
  :bind (:map neotree-mode-map
         ("M-w" . my/neotree-kill-filename-at-point)))

;; vi-tilde-fringe
(use-package vi-tilde-fringe :defer t
  :diminish vi-tilde-fringe-mode
  :hook ((prog-mode . vi-tilde-fringe-mode)))

(use-package idle-highlight-mode :defer t
  :hook (prog-mode)
  :custom
  (idle-highlight-idle-time 0.7))

;; goto-addr
(use-package goto-addr
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)))

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

;; which-key
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))

;; smooth-scroll https://github.com/k-talo/smooth-scroll.el
(use-package smooth-scroll
  :diminish smooth-scroll-mode
  :init
  (require 'smooth-scroll)
  (custom-set-variables
   '(smooth-scroll/vscroll-step-size 7))
  (smooth-scroll-mode t))

;; crux
(use-package crux
  :bind (("C-c o"   . crux-open-with)
         ("C-S-o"   . crux-smart-open-line-above)
         ("C-c n"   . crux-cleanup-buffer-or-region)
         ("C-c u"   . crux-view-url)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-c d"   . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c r"   . crux-rename-file-and-buffer)
         ("C-c M-t" . crux-visit-term-buffer)
         ("C-c k"   . crux-kill-other-buffers)
         ("C-M-z"   . crux-indent-defun)
         ("C-^"     . crux-top-join-lines)
         ("C-DEL"   . crux-kill-line-backwards)))

(use-package vlf :defer t
  :custom
  (vlf-application 'dont-ask)
  :init
  (require 'vlf-setup))

(use-package ov
  :init
  (autoload 'ov "ov.el" "Make an overlay from BEG to END.

If PROPERTIES are specified, set them for the created overlay."))

;; UCS Utility
;;(use-package ucs-utils :defer t)

;; Font Utility
;;(use-package font-utils)

;; TRAMP
(use-package tramp :defer t
  :config
  (vagrant-tramp-add-method)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;; Games:
;;(use-package gnugo :defer t)

;;; Communication:

;;; Variables:
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Functions:
(defmacro safe-diminish (file mode &optional new-name)
  "
https://github.com/larstvei/dot-emacs/blob/master/init.org"
  `(with-eval-after-load ,file
     (diminish ,mode ,new-name)))

(defvar my/disable-trailing-modes
  '(buffer-face-mode
    Buffer-menu-mode
    calendar-mode
    cfw:calendar-mode
    comint-mode
    eshell-mode
    package-menu-mode
    eww-mode
    Info-mode
    term-mode))

(use-package diminish :defer t
  :init
  (safe-diminish "face-remap" 'buffer-face-mode)
  (safe-diminish "elisp-slime-nav" 'elisp-slime-nav-mode)
  (safe-diminish "flyspell" 'flyspell-mode)
  (safe-diminish "simple" 'auto-fill-function)
  (safe-diminish "subword" 'subword-mode)
  (--each my/disable-trailing-modes
    (add-hook (intern (concat (symbol-name it) "-hook"))
              'my/disable-trailing-mode-hook)))
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
  (interactive "FFind file (as sudo): ")
  (find-file (concat "/sudo::" (replace-regexp-in-string "^sudo:[^:]*:" "" filename))))

(defun my/buffer-in-tramp ()
  "Return non-nil if buffer file is in TRAMP."
  (and buffer-file-name
       (s-match
        (concat "\\`/" (regexp-opt (mapcar 'car tramp-methods)) ":")
        buffer-file-name)))

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

(defun my/kill-buffer-file-name (n)
  "Kill buffer file name.  Return Org mode style link if `N' eq 2."
  (interactive "p")
  (let ((path (cond
               (buffer-file-name            (file-truename buffer-file-name))
               ((eq major-mode 'dired-mode) (file-truename default-directory))
               (:else                       (buffer-name)))))
    (kill-new (if (eq n 1) path
                (format "[[%s][%s]]" path (buffer-name))))))

;; Original: http://ja.stackoverflow.com/questions/12510
(defun my/insert-kbd-sequence ()
  "Insert (kbd) sequence."
  (interactive)
  (insert "(kbd \""
          (key-description (read-key-sequence "input> "))
          "\")"))
;; my/insert-kbd-sequence ends here

(defun my/insert-datetime-attr ()
  "
http://ergoemacs.org/emacs/elisp_datetime.html"
  (interactive)
  (insert
   (concat " datetime=\""
           (format-time-string "%Y-%m-%dT%T")
           (let ((x (format-time-string "%z")))
             (concat (substring x 0 3) ":" (substring x 3 5)))
           "\"")))

(defun my/find-file-temporary-file-directory (filename)
  "Find file `FILENAME' in `temporary-file-directory'."
  (interactive
   (list (read-file-name "Find files: " temporary-file-directory)))
  (find-file filename))

(defun my/php-vars-to-array ()
  ""
  (interactive)
  (let ((string (buffer-substring-no-properties (region-beginning) (region-end)))
        result)
    (setq result
          (with-temp-buffer
            (insert "[")
            (insert string)
            (insert "]")
            (goto-char (point-min))
            (message (buffer-substring-no-properties (point-min) (point-max)))
            (while (re-search-forward "\\(\\$[_a-z0-9]+\\)" nil t)
              (replace-match "'\\1' => \\1")
              (message (buffer-substring-no-properties (point-min) (point-max))))
            (buffer-substring-no-properties (point-min) (point-max))))
    (delete-region (region-beginning) (region-end))
    (insert result)))

(defun my/display-htmlize-current-buffer ()
  "Display HTMLized buffer from current buffer."
  (interactive)
  (display-buffer (htmlize-buffer)))

(defun my/kill-htmlize-current-buffer ()
  "Kill HTMLized content from current buffer."
  (interactive)
  (let ((buf (htmlize-buffer)))
    (kill-new (with-current-buffer buf
                (buffer-substring-no-properties (point-min) (point-max))))
    (kill-buffer buf)))

(defun my/mincho-face ()
  "Return Mincho anonymous face."
  `(:family ,(--first (member it (font-family-list)) '("YuMincho" "Hiragino Mincho ProN" "IPAexMincho"))))

(defun my/buffer-minchoize ()
  "Minchoize current buffer."
  (interactive)
  (require 'ov)
  (ov (point-min) (point-max) 'face (my/mincho-face)))

(defun my/insert-tetosan ()
  "Kimiwa jitsuni bakadana."
  (with-current-buffer "*scratch*"
    (goto-char (1- (point-max)))
    (insert "
;; 　　　　　 　r /
;; 　 ＿＿ , --ヽ!-- .､＿
;; 　! 　｀/::::;::::ヽ l
;; 　!二二!::／}::::丿ハﾆ|
;; 　!ﾆニ.|:／　ﾉ／ }::::}ｺ
;; 　L二lイ　　0´　0 ,':ﾉｺ
;; 　lヽﾉ/ﾍ､ ''　▽_ノイ ソ
;;  　ソ´ ／}｀ｽ /￣￣￣￣/
;; 　　　.(_:;つ/  0401 /　ｶﾀｶﾀ
;;  ￣￣￣￣￣＼/＿＿＿＿/
")))

;; Pandoc-EWW
(use-package pandoc
  :init
  (pandoc-turn-on-advice-eww))

;; init-open-recentf
(add-hook 'init-open-recentf-before-hook #'my/insert-tetosan)
(init-open-recentf)

;; Right Click
(use-package right-click-context :defer t
  :custom
  (right-click-context-mode-lighter "")
  :init
  (right-click-context-mode 1))

;; Beacon — Never lose your cursor again
(use-package beacon
  :diminish beacon-mode
  :init
  (beacon-mode 1))

;; Eshell
(add-hook 'eshell-mode-hook 'eshell-fringe-status-mode)

;; bm
(use-package bm :defer t
  :bind (("<right-fringe> <wheel-down>" . bm-next-mouse)
         ("<right-fringe> <wheel-up>"   . bm-previous-mouse)
         ("<right-fringe> <mouse-1>"    . bm-toggle-mouse)))

(use-package highlight-indent-guides-method :defer t
  :diminish highlight-indent-guides-mode
  ;; :hook ((prog-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?\|)
  (highlight-indent-guides-delay 0.5))

;; hamburger-menu
;; https://melpa.org/#/hamburger-menu
(use-package hamburger-menu :defer t
  :custom
  (hamburger-menu-symbol "ﾐ田")
  :init
  (global-hamburger-menu-mode 1))

;; pomodoro
(custom-set-variables
 '(pomodoro-sound-player "mpv"))

;; term+
;; (require 'term+)
;; (require 'xterm-256color)

;; Atomic Chrome for Emacs
;; https://github.com/alpha22jp/atomic-chrome
;; (use-package atomic-chrome :defer 3
;;   :init
;;   (atomic-chrome-start-server))

(use-package google-translate :defer t
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja"))

(defun my/reset-default-directory-by-buffer-file-name ()
  "Set default-directory by `buffer-file-name'."
  (interactive)
  (when buffer-file-name
    (setq default-directory (f-dirname buffer-file-name))))

(defun my/make-file-by-buffer (to-file from-buf)
  "Write file `TO-FILE' from opened buffer `FROM-BUF'."
  (interactive (list (read-file-name "Write to: ")
                     (read-buffer "From buffer: ")))
  (when (or (not (f-exists? to-file))
            (yes-or-no-p "Overwrite? "))
    (let ((content (with-current-buffer from-buf
                     (set-buffer-multibyte nil)
                     (setq buffer-file-coding-system 'binary)
                     (buffer-substring-no-properties (point-min) (point-max)))))
      (f-write-bytes content to-file))))

(defun my/term-mode-hook ()
  ""
  (yas-minor-mode -1))
(add-hook 'term-mode-hook 'my/term-mode-hook)

;; info
(add-to-list 'Info-default-directory-list (locate-user-emacs-file "./info/emacs-manual-24.5-ja"))

(with-eval-after-load 'dash
  (dash-enable-font-lock))

;; keyfreq
(use-package keyfreq :defer t
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; dmacro
(use-package dmacro :defer t
  :custom
  (dmacro-default-key (kbd "C-."))
  :init
  (global-dmacro-mode 1))

;; Auto deployment
(use-package copy-file-on-save :defer t
  :init
  (global-copy-file-on-save-mode 1))

(setq find-function-C-source-directory (f-expand "~/local/src/emacs/src"))

(benchmark-init/deactivate)

;; (message "Emacs finished loading (%d GCs)." gcs-done)

(provide 'init)
;;; init.el ends here
