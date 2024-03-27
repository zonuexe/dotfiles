;;; init.el --- The zonuexe's .emacs -*- coding: utf-8 ; lexical-binding: t -*-

;; Filename: init.el
;; Description: zonuexe's .emacs
;; Package-Requires: ((emacs "29.1"))
;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 2014-11-01
;; Modified: 2023-10-10
;; Keywords: internal, local
;; Human-Keywords: Emacs Initialization
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
(unless window-system
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
 '(completions-common-part ((t (:background "black" :foreground "WhiteSmoke" :slant normal :weight normal :height 1.0 :width normal))))
 '(font-lock-doc-face ((t (:slant normal)))))

(prog1 'modus-themes
  (require 'modus-themes)
  (setopt modus-themes-completions '((matches . (extrabold))
                                     (selection . (semibold italic))))
  (setopt modus-themes-italic-constructs t)
  (setopt modus-themes-bold-constructs t)
  (setopt modus-themes-prompts '(bold italic))
  (setopt modus-themes-common-palette-overrides modus-themes-preset-overrides-intense)
  (load-theme 'modus-vivendi :noconfirm))

;;; Variables:
(setq make-backup-files nil)
(setq delete-auto-save-files t)

(let ((default-directory (locate-user-emacs-file "./site-lisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(load (locate-user-emacs-file "./site-lisp/site-lisp-autoloads.el") t)
(require 'my)

;;; Packages:
(require 'borg-elpa)
(borg-elpa-initialize)

(eval-and-compile
  (require 'wiz)
  (require 'wiz-key)
  (require 'wiz-env))

(wiz wiz-pkgs
  :init
  (defvar wiz-pkgs--registerd-packages nil)
  (setopt wiz-pkgs-enable-log t))

;;; Font:
;;;     |いろはにほへと　ちりぬるを|
;;;     |わかよたれそ　　つねならむ|
;;;     |うゐのおくやま　けふこえて|
;;;     |あさきゆめみし　ゑひもせす|

(defvar my/font-family "UDEV Gothic JPDOC")
(defvar my/font-size
  (let ((size-by-hostname
         '(("tadsan-ret.local" . 16.5)
           ("tadsan-fmvzero" . 14.5))))
    (or (cdr (assoc (system-name) size-by-hostname))
        15.5)))

(when window-system
  ;; http://d.hatena.ne.jp/kitokitoki/20110502/p2
  (let ((fontset (format "%s-%.1f" my/font-family my/font-size)))
    (add-to-list 'default-frame-alist `(font . ,fontset)))
  (add-to-list 'default-frame-alist `(cursor-type . (hbar . ,(1+ (ceiling (/ my/font-size 2)))))))

;; Set and load custom-vars.el
(setq custom-file (expand-file-name "custom-vars.el" user-emacs-directory))

(require 'dash)

;;; Environment:

(setenv "CLOUDSDK_PYTHON_SITEPACKAGES" "1")

;; PATH
(unless (eval-when-compile (eq window-system 'nt))
  (wiz-envs "PATH" "TEST_SERVER" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "MANPATH" "GOROOT" "GOPATH"))

;; (when (eq window-system 'w32)
;;   (setenv "GIT_SSH" "C:\\Program Files\\PuTTY\\plink.exe"))

(when (file-directory-p "~/repo/emacs/lsp-bridge")
  (add-to-list 'load-path (expand-file-name "~/repo/emacs/lsp-bridge")))
(put 'lsp-bridge-php-lsp-server 'safe-local-variable #'stringp)

(require 'diminish)
(require 'key-chord nil t)

(defalias 'major-mode-of 'magic-filetype-major-mode-of)

(wiz nyan-mode
  :config
  (setopt nyan-bar-length 16)
  :init
  (nyan-mode 1))

;;; Coding:
(setq-default indent-tabs-mode nil)

;; White space
(setq-default show-trailing-whitespace t)

;; Uniquify
(setopt uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Show paren
(show-paren-mode t)

;; Column mode
(column-number-mode t)

;; volatile-highlights.el
(wiz volatile-highlights
  :config
  (diminish 'volatile-highlights-mode)
  :init
  (volatile-highlights-mode 1))

;; Rainbow mode
(wiz rainbow-mode
  :config
  (diminish 'rainbow-mode))

;; Key config
(wiz-keys (("M-ESC ESC"   . keyboard-quit)
           ("C-S-n"       . make-frame)
           ("C-S-w"       . delete-frame)
           ("M-N"         . untitled-new-buffer)
           ("C-M-S-d"     . projectile-dired)
           ("C-c :"       . right-click-context-menu)
           ("C-c R"       . revert-buffer)
           ("C-c S-i"     . my/display-file-info)
           ("C-x j"       . dired-jump)
           ("C-x C-S-e"   . pp-eval-last-sexp)
           ("C-x C-f"     . find-file-at-point)
           ("C-x お"      . other-window)
           ("C-S-v"       . scroll-down-command)
           ("M-o"         . ace-window)
           ("C-M-o"       . swoop-multi)
           ("M-："        . eval-expression)
           ("M-ESC ："    . eval-expression)
           ("<S-tab>"     . my/outdent-dwim)
           ("M-<left>"    . bs-cycle-previous)
           ("M-<right>"   . bs-cycle-next)
           ("C-M-S-y"     . my/kill-buffer-file-name)
           ("M-<f5>"      . compile)
           ("<f5>"        . quickrun)
           ("C-c <left>"  . windmove-left)
           ("C-c <down>"  . windmove-down)
           ("C-c <up>"    . windmove-up)
           ("C-c <right>" . windmove-right)))

(cond
 ((eq window-system 'ns)
  (when (boundp 'ns-command-modifier) (setq ns-command-modifier 'meta))
  (when (boundp 'ns-alternate-modifier) (setq ns-alternate-modifier 'meta)
        (global-set-key (kbd "M-¥") (lambda () (interactive) (insert "¥")))
        (global-set-key (kbd "¥") (lambda () (interactive) (insert "\\")))))
 ((eq window-system 'x)
  (when (boundp 'x-meta-keysym) (setq x-meta-keysym 'meta))))

;; key-chord
(wiz key-chord
  :init
  (key-chord-mode 1)
  :config
  (setopt key-chord-two-keys-delay 0.02)
  (key-chord-define-global "df" 'find-function)
  (key-chord-define-global "fh" 'describe-function)
  (key-chord-define-global "fv" 'find-variable)
  (key-chord-define-global "jb" 'jetbrains-open-buffer-file)
  (key-chord-define-global "@p" 'package-install)
  (key-chord-define-global "kl" 'align-regexp)
  (key-chord-define-global "rt" 'modus-themes-toggle)
  (key-chord-define-global "wr" 'writeroom-mode)
  (key-chord-define-global "m," 'reload-major-mode)
  (key-chord-define-global "mc" 'my/buffer-minchoize))

(savehist-mode +1)
(with-eval-after-load 'prescient
  (prescient-persist-mode +1))

(wiz vertico
  :config
  (wiz-keys (("C-l" . my-filename-upto-parent))
            :map vertico-map)
  :init
  (vertico-mode +1))

(wiz vertico-prescient
  :init
  (vertico-prescient-mode +1))

(wiz marginalia
  :init
  (marginalia-mode +1))

(wiz embark
  :init
  (wiz-keys
   (("C-c C-c" . embark-act)
    ("C-c C-o" . embark-export)
    ("C-c ?" . embark-bindings))))

(wiz consult
  :init
  (wiz-keys
   (("C-M-y" . consult-yank-from-kill-ring)
    ("C-c ;" . consult-imenu)
    ("C-c t" . consult-recent-file)
    ("C-;" . my-consult-line)
    ("M-X"  . consult-mode-command)
    ("M-g *" . consult-outline)
    ("M-t" . consult-ls-git)
    ("M-i" . consult-imenu)
    ([remap switch-to-buffer] . consult-buffer)
    ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
    ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
    ([remap goto-line] . consult-goto-line))))

(wiz embark-consult
  :init
  (with-eval-after-load 'embark
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)))

(wiz orderless
  :config
  (setopt completion-styles '(orderless basic))
  (setopt completion-category-defaults nil)
  (setopt completion-category-overrides '((file (styles . (partial-completion))))))

(wiz eldoc
  :config
  (diminish 'eldoc-mode)
  (setopt eldoc-minor-mode-string ""))

(wiz corfu
  :package (gnu corfu)
  :config
  (setopt corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setopt corfu-auto t)                 ;; Enable auto completion
  (setopt corfu-separator ?\s)          ;; Orderless field separator
  (setopt corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (setopt corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (setopt corfu-preview-current t)      ;; Disable current candidate preview
  (setopt corfu-preselect-first nil)    ;; Disable candidate preselection
  (setopt corfu-on-exact-match 'insert) ;; Configure handling of exact matches
  (setopt corfu-echo-documentation t)   ;; Disable documentation in the echo area
  (setopt corfu-scroll-margin 5)       ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; (leaf corfu-prescient :ensure t
;;   :init
;;   (corfu-prescient-mode +1))

(wiz tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :init
  ;; Setup completion at point
  (wiz-keys (("M-+" . tempel-complete) ;; Alternative tempel-expand
             ("M-*" . tempel-insert)))
  :config
  (wiz-keys (("<tab>" . tempel-next)
             ("S-<tab>" . tempel-previous))
            :map tempel-map)
  :hook-names (prog-mode-hook text-mode-hook)
  :setup-hook
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions))))

(wiz cape
  :init
  (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-tabnine))
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Magit
(wiz magit
  :init
  (wiz-keys (("C-x m" . magit-status)
             ("C-c l" . magit-blame-addition)))
  (setq-default magit-auto-revert-mode nil)
  (setq vc-handled-backends '(Git))
  (eval-after-load "vc" '(remove-hook 'find-file-hook 'vc-find-file-hook)))

(wiz gitignore-mode
  :init
  (add-to-list 'auto-mode-alist '("/\\.gitexclude\\'" "/\\.\\(?:ag\\|docker\\)?ignore\\'")))

;; EditorConfig
(wiz editorconfig
  :config
  (diminish 'editorconfig-mode)
  (setopt editorconfig-get-properties-function 'editorconfig-core-get-properties-hash)
  :init
  (editorconfig-mode t))

;; Conf-Mode
(wiz conf-mode
  :init
  (require 'generic-x)
  (add-to-list 'auto-mode-alist '("/\\.env\\(?:\\.sample\\)?\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("/\\.*conf\\(?:ig\\)?\\'" . conf-mode) t)
  (add-to-list 'auto-mode-alist '("/\\.*rc\\'" . conf-mode) t))

;; Projectile
(wiz projectile
  :config
  (setopt projectile-enable-caching nil))

;; Flycheck
(wiz flycheck-posframe
  :config
  (lambda ()
    (setopt flycheck-posframe-border-width 7)
    (setopt flycheck-posframe-warning-prefix "\u26a0 ")
    (setopt flycheck-posframe-position 'window-center)))

(wiz flycheck
  :config
  (lambda ()
    (diminish 'flycheck-mode)
    (add-hook 'flycheck-mode-hook 'flycheck-cask-setup)
    (add-hook 'flycheck-mode-hook 'flycheck-eask-setup)
    (add-hook 'flycheck-mode-hook 'flycheck-posframe-mode))
  :init
  (lambda ()
    (require 'flycheck-posframe)
    (global-flycheck-mode t)
    (flycheck-package-setup)
    (with-current-buffer (get-buffer-create flycheck-posframe-buffer)
      (goto-address-mode +1))))

;; elec-pair
(wiz elec-pair
  :init
  (electric-pair-mode 1))

;; which-func
(wiz which-func
  :init
  (which-function-mode 1))

;; smartchr
(wiz smartchr
  :init
  (autoload 'smartchr "smartchr"
    "Make an interactive command to support input several LIST-OF-STRING candidates."
    t))

(defun my-presentation-on ()
  t)

(defun my-presentation-off ()
  t)

(wiz presentation
  :config
  (add-hook 'presentation-on  #'my-presentation-on)
  (add-hook 'presentation-off #'my-presentation-off))

;;; Languages:
(wiz sql
  :config
  (lambda ()
    (setopt sql-product 'mysql)))

;; Web


(defun sp-web-mode-is-code-context (_id action _context)
  "This snippet is derived from http://web-mode.org/ ."
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(wiz web-mode
  :init
  (lambda ()
    (mapc (lambda (it) (add-to-list 'auto-mode-alist (cons it #'web-mode)))
          (list "\\.html?\\'" "\\.tpl\\'" "\\.tpl\\.xhtml\\'" "\\.ejs\\'" "\\.hbs\\'" "\\.jsx\\'"
                "\\(\\.html\\)?\\.erb\\'" "\\.tsx\\'" "\\.vue\\'")))
  :config
  (lambda ()
    (setopt web-mode-enable-auto-pairing nil)
    (setopt web-mode-enable-auto-indentation nil)
    (require 'smartparens)
    (flycheck-add-mode 'typescript-tslint 'web-mode)
    (add-to-list 'web-mode-ac-sources-alist
                 '("html" . (ac-source-html-tag ac-source-html-attr ac-source-html-attrv)))
    (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context)))
  :setup-hook
  (defun init-web-mode-setup ()
    "Set variables for web-mode."
    (emmet-mode +1)
    (when (and buffer-file-name (string= "tsx" (file-name-extension buffer-file-name)))
      (my-setup-typescript))))

;; PHP
(defun my/turn-on-php-eldoc ()
  "Turn on php-eldoc."
  (when (require 'php-eldoc nil t)
    (php-eldoc-enable)))

(add-to-list 'auto-minor-mode-alist '("/pixiv/" . pixiv-dev-mode))

(wiz php
  :hook-names (php-base-mode-hook)
  :setup-hook
  (defun init-php-base-mode-setup ()
    (make-local-variable 'treesit-font-lock-level)
    (setopt treesit-font-lock-level 4)))

(wiz php-mode
  :load-if-exists "~/repo/emacs/php-mode/lisp/php-mode-autoloads.el"
  :hook-names (php-mode-hook)
  :config
  (lambda ()
    (setopt php-default-major-mode 'php-mode)
    (setopt php-manual-url 'ja)
    (setopt php-mode-coding-style 'psr2)
    (setopt php-mode-template-compatibility nil)
    (setopt php-imenu-generic-expression 'php-imenu-generic-expression-simple)
    (setopt php-project-auto-detect-etags-file t)
    (setopt php-ide-mode nil)
    (setopt phpstan-memory-limit "2G")

    (require 'flycheck-phpstan)
    (flycheck-add-next-checker 'php 'phpstan)
    (when (require 'flycheck-psalm nil t)
      (flycheck-add-next-checker 'php 'psalm))
    (phpactor-smart-jump-register)

    (wiz-keys (("[" . (smartchr "[`!!']" "array(`!!')" "[[`!!']]"))
               ("]" . (smartchr "array " "]" "]]"))
               ("&" . (smartchr "&" "&& "))
               ("\\" . (smartchr "\\" "\\PHPStan\\dumpType(`!!');" "\\\\"))
               ("¥" . (smartchr "\\" "\\PHPStan\\dumpType(`!!');" "\\\\"))
               ("|" . (smartchr "|" "|| " ))
               ("." . (smartchr
                       (my-php-smartchr-dot "->" "." ". ")
                       (my-php-smartchr-dot ". " ".." "..")
                       "..."))
               ("^" . (smartchr "^" "fn() => " "function () {`!!'}"))
               ("@" . (smartchr "@" "$this->"))
               ("~" . (smartchr "~" "phpstan-"))
               ("C-c C-c" . 'psysh-eval-region)
               ("<f6>" . phpunit-current-project)
               ("C-c C--" . php-current-class)
               ("C-c C-=" . 'php-current-namespace))
              :map php-mode-map))
  :setup-hook
  (defun init-php-mode-setup ()
    "My PHP-mode hook."
    ;;(require 'company-phpactor)
    (my/turn-on-php-eldoc)
    (subword-mode 1)
    (add-hook 'hack-local-variables-hook 'php-ide-turn-on nil t)
    (setq show-trailing-whitespace t)

    (setq-local ac-disable-faces '(font-lock-comment-face font-lock-string-face php-string))

    (flycheck-mode t)
    (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
    (add-to-list 'flycheck-disabled-checkers 'php-phpcs)

    (when (and buffer-file-name (string-match-p "/pixiv/" buffer-file-name))
      (require 'pixiv-dev nil t)
      (add-to-list 'flycheck-disabled-checkers 'psalm)
      (pixiv-dev-mode t))

    (php-format-auto-mode +1)

    (setq-local completion-at-point-functions
                (append (list #'php-complete-complete-function #'tempel-complete)
                        (list (cape-company-to-capf #'company-phpactor))
                        completion-at-point-functions))

    (when (eq 0 (buffer-size))
      (insert "<?php\n\n"))))

(wiz lsp-bridge
  :config
  (lambda ()
    (setopt lsp-bridge-completion-popup-predicates
            (cl-nset-difference lsp-bridge-completion-popup-predicates
                                '(lsp-bridge-not-in-string lsp-bridge-not-in-comment)))))

(wiz php-ide
  :hook-names (php-ide-mode-functions)
  :setup-hook
  (defun init-php-ide-mode (feature activate)
    "Customize php-ide."
    (pcase feature
      (`lsp-bridge
       (if activate
           (progn (yas-minor-mode +1)
                  (corfu-mode -1))
         (yas-minor-mode -1)
         (corfu-mode +1))))))

(wiz company
  :config
  (lambda ()
    (require 'company-tabnine)
    (add-to-list 'company-backends #'company-tabnine)))

(wiz psysh
  :config
  (lambda ()
    (add-hook 'psysh-mode #'my/turn-on-php-eldoc)
    (setopt psysh-doc-display-function #'popwin:display-buffer)))

(add-to-list 'auto-mode-alist `("/composer.lock\\'" . ,(major-mode-of 'json)))
(add-to-list 'auto-mode-alist '("\\.php\\.faces\\'" . lisp-data-mode))

(wiz pixiv-dev
  :config
  (lambda ()
    (setopt pixiv-dev-user-name "tadsan"))
  :init
  (lambda ()
    (autoload 'pixiv-dev-shell "pixiv-dev" nil t)
    (autoload 'pixiv-dev-find-file "pixiv-dev" nil t)
    (autoload 'pixiv-dev-copy-file-url "pixiv-dev" nil t)))

(wiz phan
  :init
  (lambda ()
    (add-to-list 'auto-mode-alist
                 '("/\\(phan\\|filter\\)\\(?:-.+\\)?\\.log\\'" . phan-log-mode))))

;; (leaf dumb-jump :ensure t
;;   :init
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(defun my-emacs-lisp-mode-setup ()
  "Setup function for Emacs Lisp."
  (rainbow-mode t)
  (set-face-foreground 'font-lock-regexp-grouping-backslash "indian red")
  (set-face-foreground 'font-lock-regexp-grouping-construct "peru")
  (nameless-mode t)
  (turn-on-eldoc-mode)
  (elisp-slime-nav-mode +1)
  (when (eq major-mode 'inferior-emacs-lisp-mode)
    (wiz-keys (("<RET>" . #'ielm-return))
              :map ielm-map)))

(wiz nameless
  :config
  (lambda ()
    (diminish 'nameless-mode)
    (add-to-list 'nameless-global-aliases '("pv" . "projectile-variable"))))

(defvar my/emacs-lisp-modes
  '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook lisp-data-mode-hook))
(--each my/emacs-lisp-modes
  (add-hook it #'my-emacs-lisp-mode-setup))

;; `Cask' is NOT emacs-lisp-mode
(add-to-list 'auto-mode-alist '("/Cask\\'" . lisp-mode))

(wiz lsp-mode
  :config
  (lambda ()
    (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
    (setopt lsp-completion-provider :none)
    (setopt lsp-ui-doc-use-childframe nil)))

(wiz paredit
  :config
  (lambda ()
    (diminish 'paredit-mode)
    (wiz-keys (("C-<right>" . right-word)
	       ("C-<left>"  . left-word))
              :map paredit-mode-map))
  :init
  (lambda ()
    (mapc (lambda (it) (add-hook it 'enable-paredit-mode)) my/emacs-lisp-modes)))

;; Scheme

(wiz scheme
  :hook-names (scheme-mode-hook)
  :config
  (lambda ()
    (setopt geiser-active-implementations '(guile racket)))
  :setup-hook
  (defun init-scheme-mode-setup ()
    "λ..."
    (paredit-mode t)))

;; Haskell
(wiz haskell-mode
  :config
  (lambda ()
    (add-hook 'haskell-mode-hook #'turn-on-eldoc-mode)
    (add-hook 'haskell-mode-hook #'turn-on-haskell-indent)))

;; TypeScript
(defun my-setup-typescript ()
  "Setup function for TypeScript."
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(wiz typescript-mode
  :init
  (lambda ()
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))
  :config
  (lambda ()
    (add-hook 'typescript-mode-hook #'my-setup-typescript)))

;; text-mode
(wiz diff-mode
  :init
  (lambda ()
    (add-to-list 'auto-mode-alist '("/infection.log" . diff-mode))))

(wiz text-mode
  :init
  (lambda ()
    (add-to-list 'auto-mode-alist '("/LICENSE\\'" . text-mode)))
  :setup-hook
  (defun init-text-mode-setup ()
    "Setup function for `text-mode'."
    (unless (eq major-mode 'html-mode)
      (setq line-spacing 3))))

;; YAML
(wiz yaml-mode
  :init
  (lambda ()
    (add-to-list 'auto-mode-alist '("/\\.gemrc\\'" . yaml-mode))))

;; Markdown Mode
(wiz markdown-mode
  :init
  (lambda ()
    (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))
  :config
  (lambda ()
    (setopt markdown-command '("pandoc" "--from=markdown" "--to=html5"))
    (setopt markdown-fontify-code-blocks-natively t)
    (setopt markdown-header-scaling t)
    (setopt markdown-indent-on-enter 'indent-and-new-item)

    (define-key markdown-mode-map (kbd "<S-tab>") #'markdown-shifttab))
  :setup-hook
  (defun init-markdown-mode-setup ()
    (visual-line-mode nil)))

;; Magic Filetype
(wiz magic-filetype
  :init
  (lambda ()
    (magic-filetype-set-auto-mode 'ruby)
    (magic-filetype-enable-vim-filetype)))

;;; Others:

;; Save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Recentf
(wiz recentf
  :config
  (lambda ()
    (setopt recentf-max-saved-items 2000)
    (setopt recentf-auto-cleanup 'never)
    (setopt recentf-exclude
            '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.cache/"
              "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\(\\.cask/\\|bookmarks\\)"
              "/elpa/.*-autoloads\\.el\\'"  "/\\newsrc\\(\\.eld\\)?\\'")))
  :init
  (lambda ()
    (recentf-mode t)
    (run-with-idle-timer 30 t #'recentf-save-list)))

;; Undo
(wiz undo-fu
  :init
  (lambda ()
    (wiz-keys (("C-_" . undo-fu-only-undo)
               ("C-?" . undo-fu-only-redo)))))

;; expand-region.el
(wiz expand-region
  :config
  (lambda ()
    (wiz-keys (("C-@" . er/expand-region)
               ("C-`" . er/contract-region)))))

;; Annotate.el
(wiz annotate
  :config
  (lambda ()
    (wiz-keys (("M-@"   . annotate-annotate)
               ("C-M-@" . annotate-clear-annotations)))))

;;; Tools:

;; Open junk file
(wiz open-junk-file
  :init
  (lambda ()
    (wiz-keys (("C-c j" . open-junk-file))))
  :config
  (lambda ()
    (setopt open-junk-file-format "~/junk/%Y/%m/%Y-%m-%d-%H%M%S-")))

;; restclient.el
(wiz restclient
  :init
  (lambda ()
    (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))))

(wiz org
  :init
  (lambda ()
    (wiz-keys (("C-c c" . 'org-capture)))
    (autoload 'ioslide:helper "ox-ioslide-helper.el" "Key menu for ioslide" t))
  :config
  (lambda ()
    (setopt org-default-notes-file (concat org-directory "/capture.org"))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)))))

;; Org-IO Slide
;;(require 'ox-ioslide-helper)

;; ElScreen
(wiz elscreen
  :config
  (lambda ()
    (setopt elscreen-prefix-key (kbd "C-z"))
    (setopt elscreen-display-tab nil)
    (setopt elscreen-tab-display-kill-screen nil)
    (setopt elscreen-tab-display-control nil))
  :init
  (lambda ()
    (wiz-keys (("C-<tab>" . elscreen-next)
               ("<C-iso-lefttab>" . elscreen-previous)))
    (elscreen-start)
    ;; El-Screeのウィンドウを一個つくる
    (elscreen-create)))

(wiz ctrlf
  :config
  (lambda ()
    (diminish 'ctrlf-mode))
  :init
  (lambda ()
    (ctrlf-mode +1)))

(wiz rg
  :init
  (lambda ()
    (wiz-keys (("C-:" . rg)
               ("M-C-:" . rg-literal))))
  :config
  (lambda ()
    (add-hook 'rg-mode-hook #'wgrep-rg-setup)))

;; direx
(wiz direx
  :init
  (lambda ()
    (wiz-keys (("M-C-\\" . direx-project:jump-to-project-root-other-window)
               ("M-C-¥"  . direx-project:jump-to-project-root-other-window)))))

;; dired-k
(wiz dired-k
  :config
  (lambda ()
    (wiz-keys (("K" . dired-k))
              :map dired-mode-map))
  :init
  (lambda ()
    (add-hook 'dired-initial-position-hook #'dired-k)))

(wiz dired
  :config
  (lambda ()
    (add-hook 'dired-mode-hook #'dired-preview-mode)))

;; Visual
(wiz visual-regexp
  :init
  (lambda ()
    (wiz-keys (("M-%" . vr/query-replace)))))

;; image-mode
(wiz image-mode
  :config
  (lambda ()
    (wiz-keys (("<wheel-up>"    . image-previous-line)
               ("<wheel-down>"  . image-next-line)
               ("<wheel-right>" . image-forward-hscroll)
               ("<wheel-left>"  . image-backward-hscroll))
              :map image-mode-map)))

;; Yet another folding
(wiz yafolding
  :init
  (lambda ()
    (add-hook 'prog-mode-hook 'yafolding-mode)))

;; vi-tilde-fringe
(wiz vi-tilde-fringe
  :config
  (lambda ()
    (diminish 'vi-tilde-fringe-mode))
  :init
  (lambda ()
    (add-hook 'prog-mode-hook 'vi-tilde-fringe-mode)))

(prog1 'goto-addr
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (add-hook 'text-mode-hook #'goto-address-mode))

;; (leaf smart-jump :ensure t)

;; multiple-cursors
;; http://qiita.com/ongaeshi/items/3521b814aa4bf162181d
(wiz multiple-cursors
  :init
  (lambda ()
    (require 'smartrep)
    (declare-function smartrep-define-key "smartrep")
    (wiz-keys (("C-M-c" . mc/edit-lines)
               ("C-M-r" . mc/mark-all-in-region)))
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
        ("O"   . 'mc/reverse-regions)))))

;; which-key
(wiz which-key
  :config
  (lambda ()
    (diminish 'which-key-mode)
    (setopt which-key-idle-delay 1.5))
  :init
  (lambda ()
    (which-key-setup-side-window-right-bottom)
    (which-key-mode t)))

;; smooth-scroll https://github.com/k-talo/smooth-scroll.el
(wiz smooth-scroll
  :config
  (lambda ()
    (diminish 'smooth-scroll-mode)
    (setopt smooth-scroll/vscroll-step-size 7))
  :init
  (lambda ()
    (require 'smooth-scroll)
    (smooth-scroll-mode t)))

(wiz topsy
  :init
  (lambda ()
    (add-hook 'prog-mode-hook #'topsy-mode)))

(wiz puni
  :init
  (lambda ()
    (wiz-keys (("C-)"         . puni-slurp-forward)
               ;; ("C-<right>" . puni-slurp-forward)
               ("C-}"         . puni-barf-forward)
               ;; ("C-<left>" . puni-barf-forward)
               ("C-("         . puni-slurp-backward)
               ("C-M-<left>"  . puni-slurp-backward)
               ("C-{"         . puni-barf-backward)
               ("C-M-<right>" . puni-barf-backward)
               ("M-<up>"      . puni-splice-killing-backward)
               ("M-<down>"    . puni-splice-killing-forward)
               ("ESC <up>"    . puni-splice-killing-backward)
               ("ESC <down>"  . puni-splice-killing-forward))
              :map puni-mode-map)
    (with-eval-after-load 'php-mode
      (add-hook 'php-mode-hook 'puni-mode))
    (with-eval-after-load 'web-mode
      (add-hook 'web-mode-hook 'puni-mode))))

;; crux
(wiz crux
  :init
  (lambda ()
    (wiz-keys
     (("C-c o"   . crux-open-with)
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
      ("C-DEL"   . crux-kill-line-backwards)))))

(wiz vlf
  :config
  (lambda ()
    (setopt vlf-application 'dont-ask))
  :init
  (lambda ()
    (require 'vlf-setup)))

(wiz ov
  :init
  (lambda ()
    (autoload 'ov "ov.el" "Make an overlay from BEG to END.

If PROPERTIES are specified, set them for the created overlay.")))

(wiz writeroom-mode
  :config
  (lambda ()
    (setopt writeroom-fringes-outside-margins t)
    (setopt writeroom-maximize-window nil)
    (setopt writeroom-global-effects '())))

(wiz nov
  :init
  (lambda ()
    (add-hook 'nov-post-html-render-hook #'writeroom-mode)
    (add-hook 'nov-post-html-render-hook #'my/disable-trailing-mode-hook)))

;; TRAMP
(wiz tramp
  :config
  (lambda ()
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))

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

(wiz diminish
  :init
  (lambda ()
    (safe-diminish "face-remap" 'buffer-face-mode)
    (safe-diminish "elisp-slime-nav" 'elisp-slime-nav-mode)
    (safe-diminish "flyspell" 'flyspell-mode)
    (safe-diminish "simple" 'auto-fill-function)
    (safe-diminish "subword" 'subword-mode)
    (safe-diminish "gcmh" 'gcmh-mode)))

(wiz-map my/disable-trailing-modes
  (lambda (it)
    `(add-hook (quote ,(intern (concat (symbol-name it) "-hook")))
               #'my/disable-trailing-mode-hook)))

;;; My Functions:
(defun reload-major-mode ()
  "Reload current major mode."
  (interactive)
  (let ((current-mode major-mode))
    (prog1 current-mode
      (fundamental-mode)
      (funcall current-mode))))

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
         (save-excursion (goto-char (region-beginning)) (line-beginning-position))
         (save-excursion (goto-char (region-end)) (line-end-position))
         offset)
      (indent-rigidly (line-beginning-position) (line-end-position) offset))))
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
    (unless buffer-read-only
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
"))))

;; Pandoc-EWW
(wiz pandoc
  :init
  (pandoc-turn-on-advice-eww))

;; init-open-recentf
(when (eval-when-compile (file-directory-p "~/repo/emacs/init-open-recentf.el/"))
  (load "~/repo/emacs/init-open-recentf.el/init-open-recentf.el"))
(add-hook 'init-open-recentf-before-hook #'my/insert-tetosan)
(init-open-recentf)

;; Right Click
(wiz right-click-context
  :config
  (setopt right-click-context-mode-lighter "")
  :init
  (right-click-context-mode 1))

(wiz pulsar
  :config
  (diminish 'pulsar-mode)
  :init
  (pulsar-global-mode 1))

;; Eshell
(add-hook 'eshell-mode-hook 'eshell-fringe-status-mode)

(wiz highlight-indent-guides
  :config
  (diminish 'highlight-indent-guides-mode)
  (setopt highlight-indent-guides-method 'character)
  (setopt highlight-indent-guides-character ?\|)
  (setopt highlight-indent-guides-delay 0.5))

(wiz google-translate-default-ui
  :config
  (setopt google-translate-default-source-language "en")
  (setopt google-translate-default-target-language "ja"))

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

(wiz eat
  :config
  (add-hook 'eat-mode-hook #'my/disable-trailing-mode-hook))

;; keyfreq
(wiz keyfreq
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Auto deployment
(wiz copy-file-on-save
  :init
  (global-copy-file-on-save-mode 1))

(setq find-function-C-source-directory
      (eval-when-compile (expand-file-name "~/local/src/emacs/src")))

(when (eval-when-compile my-system-is-wsl2)
  (setopt browse-url-browser-function #'my-browse-url-wsl-host-browser))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(gcmh-mode 1)

;; (message "Emacs finished loading (%d GCs)." gcs-done)

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
