;;; init.el --- The zonuexe's .emacs -*- coding: utf-8 ; lexical-binding: t -*-

;; Filename: init.el
;; Description: zonuexe's .emacs
;; Package-Requires: ((emacs "26.1"))
;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 2014-11-01
;; Modified: 2022-10-10
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

(leaf modus-themes
  :custom
  (modus-themes-completions '((matches . (extrabold))
                              (vertico . (semibold accented))
                              (popup . (accented intense))))
  (modus-themes-italic-constructs . t)
  (modus-themes-bold-constructs . t)
  (modus-themes-prompts . '(background bold gray intense italic))
  :init
  (require 'modus-themes)
  :config
  (custom-set-variables
   '(modus-themes-common-palette-overrides modus-themes-preset-overrides-intense))
  (load-theme 'modus-vivendi :noconfirm))

;;; Variables:
(setq make-backup-files nil)
(setq delete-auto-save-files t)

(let ((default-directory (locate-user-emacs-file "./site-lisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(load (locate-user-emacs-file "./site-lisp/site-lisp-autoloads.el") t)

;;; Packages:
(require 'borg-elpa)
(borg-elpa-initialize)

;;; Font:
;;;     |いろはにほへと　ちりぬるを|
;;;     |わかよたれそ　　つねならむ|
;;;     |うゐのおくやま　けふこえて|
;;;     |あさきゆめみし　ゑひもせす|

(defvar my/font-family "Migu 2M")
(defvar my/font-size
  (let ((size-by-hostname
         '(("tadsan-ret.local" . 16.5))))
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

;; PATH
(custom-set-variables
 '(exec-path-from-shell-check-startup-files nil)
 '(exec-path-from-shell-variables '("PATH" "TEST_SERVER" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "MANPATH" "GOROOT" "GOPATH")))

(setenv "CLOUDSDK_PYTHON_SITEPACKAGES" "1")

(unless (eq window-system 'nt)
  (exec-path-from-shell-initialize))

;; (when (eq window-system 'w32)
;;   (setenv "GIT_SSH" "C:\\Program Files\\PuTTY\\plink.exe"))

(when (file-directory-p "~/repo/emacs/php-mode")
  (load "~/repo/emacs/php-mode/lisp/php-mode-autoloads.el"))

(when (file-directory-p "~/repo/emacs/lsp-bridge")
  (add-to-list 'load-path (expand-file-name "~/repo/emacs/lsp-bridge")))
(put 'lsp-bridge-php-lsp-server 'safe-local-variable #'stringp)

(eval-when-compile
  (require 'leaf))

(require 'diminish)
(require 'bind-key)
(require 'key-chord)

(leaf-keywords-init)

(defalias 'major-mode-of 'magic-filetype-major-mode-of)

(leaf nyan-mode
  :custom
  (nyan-bar-length . 16)
  :init
  (nyan-mode 1))

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
(leaf volatile-highlights
  :diminish volatile-highlights-mode
  :init
  (volatile-highlights-mode t))

;; Rainbow mode
(leaf rainbow-mode
  :diminish rainbow-mode)

;; Key config
(progn
  (bind-key  "M-ESC ESC"   'keyboard-quit)
  (bind-key  "C-S-n"       'make-frame)
  (bind-key  "C-S-w"       'delete-frame)
  (bind-key  "M-N"         'untitled-new-buffer)
  (bind-key  "C-M-S-d"     'projectile-dired)
  (bind-key  "C-c :"       'right-click-context-menu)
  (bind-key  "C-c R"       'revert-buffer)
  (bind-key  "C-c S-i"     'my/display-file-info)
  (bind-key  "C-x j"       'dired-jump)
  (bind-key  "C-x C-S-e"   'pp-eval-last-sexp)
  (global-set-key (kbd "C-x C-f") #'find-file-at-point)
  (bind-key  "C-x お"      'other-window)
  (bind-key  "C-S-v"       'scroll-down-command)
  (bind-key  "M-o"         'ace-window)
  (bind-key  "C-M-o"       'swoop-multi)
  (bind-key  "M-："        'eval-expression)
  (bind-key  "M-ESC ："    'eval-expression)
  (bind-key  "<S-tab>"     'my/outdent-dwim)
  (bind-key  "M-<left>"    'bs-cycle-previous)
  (bind-key  "M-<right>"   'bs-cycle-next)
  (bind-key  "C-M-S-y"     'my/kill-buffer-file-name)
  (bind-key  "M-<f5>"      'compile)
  (bind-key  "<f5>"        'quickrun)
  (bind-key "C-c <left>"  'windmove-left)
  (bind-key "C-c <down>"  'windmove-down)
  (bind-key "C-c <up>"    'windmove-up)
  (bind-key "C-c <right>" 'windmove-right))
(cond
 ((eq window-system 'ns)
  (when (boundp 'ns-command-modifier) (setq ns-command-modifier 'meta))
  (when (boundp 'ns-alternate-modifier) (setq ns-alternate-modifier 'meta)
  (global-set-key (kbd "M-¥") (lambda () (interactive) (insert "¥")))
  (global-set-key (kbd "¥") (lambda () (interactive) (insert "\\")))))
 ((eq window-system 'x)
  (when (boundp 'x-meta-keysym) (setq x-meta-keysym 'meta))
  (when (boundp 'x-meta-keysym) (setq x-meta-keysym 'meta))))

;; key-chord
(leaf key-chord
  :custom
  (key-chord-two-keys-delay . 0.02)
  :init
  (key-chord-mode 1)
  :config
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

(leaf vertico :ensure t
  :bind (:vertico-map (("C-l" . my-filename-upto-parent)))
  :init
  (vertico-mode +1))

(leaf vertico-prescient :ensure t
  :init
  (vertico-prescient-mode +1))

(leaf marginalia :ensure t
  :init
  (marginalia-mode +1))

(leaf embark :ensure t
  :bind (("C-c C-c" . embark-act)
         ("C-c C-o" . embark-export)
         ("C-c ?" . embark-bindings)))

(leaf consult :ensure t
  :bind (("C-M-y" . consult-yank-from-kill-ring)
         ("C-c ;" . consult-imenu)
         ("C-c t" . consult-recent-file)
         ("C-;" . my-consult-line)
         ("M-X"  . consult-mode-command)
         ("M-g *" . consult-outline)
         ("M-t" . consult-ls-git)
         ("M-i" . consult-imenu))
  :init
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line))

(leaf embark-consult :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(leaf orderless :ensure t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles . (partial-completion))))))

(leaf eldoc
  :diminish eldoc-mode
  :custom
  (eldoc-minor-mode-string . ""))

(leaf corfu :ensure t
  :custom
  (corfu-cycle . t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto . t)                 ;; Enable auto completion
  (corfu-separator . ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary . nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match . nil)      ;; Never quit, even if there is no match
  (corfu-preview-current . t)      ;; Disable current candidate preview
  (corfu-preselect-first . nil)    ;; Disable candidate preselection
  (corfu-on-exact-match . t)       ;; Configure handling of exact matches
  (corfu-echo-documentation . t)   ;; Disable documentation in the echo area
  (corfu-scroll-margin . 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(leaf corfu-prescient :ensure t
  :init
  (corfu-prescient-mode +1))

(leaf tempel :ensure t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind
  ("M-+" . tempel-complete) ;; Alternative tempel-expand
  ("M-*" . tempel-insert)
  (:tempel-map
   ("<tab>" . tempel-next)
   ("S-<tab>" . tempel-previous))

  :init
  ;; Setup completion at point
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
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(leaf cape :ensure t
  :init
  (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-tabnine))
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Magit
(leaf magit
  :bind (("C-x m" . magit-status)
         ("C-c l" . magit-blame-addition))
  :init
  (setq-default magit-auto-revert-mode nil)
  (setq vc-handled-backends '())
  (eval-after-load "vc" '(remove-hook 'find-file-hook 'vc-find-file-hook)))

(leaf gitignore-mode
  :mode ("/\\.gitexclude\\'" "/\\.\\(?:ag\\|docker\\)?ignore\\'"))

;; EditorConfig
(leaf editorconfig
  :diminish editorconfig-mode
  :custom
  (editorconfig-get-properties-function . 'editorconfig-core-get-properties-hash)
  :init
  (editorconfig-mode t))

;; Conf-Mode
(leaf conf-mode
  :init
  (require 'generic-x)
  (add-to-list 'auto-mode-alist '("/\\.env\\(?:\\.sample\\)?\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("/\\.*conf\\(?:ig\\)?\\'" . conf-mode) t)
  (add-to-list 'auto-mode-alist '("/\\.*rc\\'" . conf-mode) t))

;; Projectile
(leaf projectile
  :custom
  (projectile-enable-caching . nil))

;; Flycheck
(leaf flycheck
  :diminish flycheck-mode
  :hook ((flycheck-mode-hook . flycheck-cask-setup))
  :init
  (global-flycheck-mode t)
  (flycheck-package-setup))

;; elec-pair
(leaf elec-pair
  :init
  (electric-pair-mode 1))

;; which-func
(leaf which-func
  :init
  (which-function-mode 1))

;; smartchr
(leaf smartchr
  :commands smartchr)

(defun my-presentation-on ()
  t)

(defun my-presentation-off ()
  t)

(leaf presentation-mode
  :hook ((presentation-on  . my-presentation-on)
         (presentation-off . my-presentation-off)))

;;; Languages:
(leaf sql
  :custom
  (sql-product . 'mysql))

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

(leaf web-mode
  :hook ((web-mode . my-web-mode-setup))
  :mode
  ("\\.html?\\'" "\\.tpl\\'" "\\.tpl\\.xhtml\\'" "\\.ejs\\'" "\\.hbs\\'" "\\.jsx\\'"
   "\\(\\.html\\)?\\.erb\\'" "\\.tsx\\'" "\\.vue\\'")
  :custom
  (web-mode-enable-auto-pairing . nil)
  (web-mode-enable-auto-indentation . nil)
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
  (when (fboundp 'php-ide-mode)
    (add-hook 'hack-local-variables-hook 'php-ide-turn-on nil t))
  (setq show-trailing-whitespace t)

  (setq-local ac-disable-faces '(font-lock-comment-face font-lock-string-face php-string))

  (flycheck-mode t)
  (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
  (add-to-list 'flycheck-disabled-checkers 'php-phpcs)

  (when (and buffer-file-name (string-match-p "/pixiv/" buffer-file-name))
    (require 'pixiv-dev nil t)
    (add-to-list 'flycheck-disabled-checkers 'psalm)
    (pixiv-dev-mode t))

  (setq-local completion-at-point-functions
              (append (list #'php-complete-complete-function)
                      (list (cape-company-to-capf #'company-phpactor))
                      completion-at-point-functions))

  (when (eq 0 (buffer-size))
    (insert "<?php\n\n")))

(add-to-list 'auto-minor-mode-alist '("/pixiv/" . pixiv-dev-mode))

(leaf php-mode
  :hook ((php-mode-hook . my-php-mode-setup))
  :custom
  (php-default-major-mode . 'php-mode)
  (php-manual-url . 'ja)
  (php-mode-coding-style . 'psr2)
  (php-mode-template-compatibility . nil)
  (php-project-auto-detect-etags-file . t)
  (phpstan-memory-limit . "2G")
  :config
  (require 'flycheck-phpstan)
  (flycheck-add-next-checker 'php 'phpstan)
  (when (require 'flycheck-psalm nil t)
    (flycheck-add-next-checker 'php 'psalm))
  (phpactor-smart-jump-register)

  (bind-key "[" (smartchr "[`!!']" "array(`!!')" "[[`!!']]") php-mode-map)
  (bind-key "]" (smartchr "array " "]" "]]") php-mode-map)
  (bind-key "&" (smartchr "&" "&& ") php-mode-map)
  (bind-key "|" (smartchr "|" "|| " ) php-mode-map)
  (bind-key "^" (smartchr "^" "fn() => " "function () {`!!'}") php-mode-map)
  (bind-key "@" (smartchr "@" "$this->") php-mode-map)
  (bind-key "C-c C-c" 'psysh-eval-region         php-mode-map)
  (bind-key "<f6>" 'phpunit-current-project      php-mode-map)
  (bind-key "C-c C--" 'php-current-class php-mode-map)
  (bind-key "C-c C-=" 'php-current-namespace php-mode-map))

(with-eval-after-load "company"
  (require 'company-tabnine)
  (add-to-list 'company-backends #'company-tabnine))

(leaf psysh
  :custom
  (psysh-doc-display-function . #'popwin:display-buffer))

(add-to-list 'auto-mode-alist `("/composer.lock\\'" . ,(major-mode-of 'json)))

(leaf psysh
  :hook ((psysh-mode . my/turn-on-php-eldoc)))

(leaf pixiv-dev
  :custom
  (pixiv-dev-user-name . "tadsan")
  :init
  (autoload 'pixiv-dev-shell "pixiv-dev" nil t)
  (autoload 'pixiv-dev-find-file "pixiv-dev" nil t)
  (autoload 'pixiv-dev-copy-file-url "pixiv-dev" nil t))

(leaf phan
  :mode (("/\\(phan\\|filter\\)\\(?:-.+\\)?\\.log\\'" . phan-log-mode)))

;; (leaf dumb-jump :ensure t
;;   :init
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Ruby
(defun my-enh-ruby-mode-setup ()
  "Setup function for `enh-ruby-mode'."
  (setq-local ac-ignore-case t))

(leaf enh-ruby-mode
  :mode (("\\.rb\\'" . enh-ruby-mode))
  :hook ((enh-ruby-mode . my-enh-ruby-mode-setup))
  :config
  (subword-mode t)
  (yard-mode t)
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
(leaf inf-ruby
  :hook ((inf-ruby-mode . ansi-color-for-comint-mode-on)))

;; Python
(leaf python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; Lisp
;; (defvar my/emacs-lisp-ac-sources
;;   '(ac-source-features ac-source-functions ac-source-variables ac-source-symbols))

(defun my-emacs-lisp-mode-setup ()
  "Setup function for Emacs Lisp."
  (rainbow-mode t)
  (set-face-foreground 'font-lock-regexp-grouping-backslash "indian red")
  (set-face-foreground 'font-lock-regexp-grouping-construct "peru")
  (nameless-mode t)
  (turn-on-eldoc-mode)
  (elisp-slime-nav-mode +1))

(leaf nameless
  :diminish nameless-mode
  :config
  (with-eval-after-load 'nameless
    (add-to-list 'nameless-global-aliases '("pv" . "projectile-variable"))))

(defvar my/emacs-lisp-modes
  '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook lisp-data-mode-hook))
(--each my/emacs-lisp-modes
  (add-hook it #'my-emacs-lisp-mode-setup))

(add-hook 'lisp-interaction-mode-hook #'turn-on-orgtbl)

;; `Cask' is NOT emacs-lisp-mode
(add-to-list 'auto-mode-alist '("/Cask\\'" . lisp-mode))

(leaf lsp-mode
  :hook ((lsp-after-open . lsp-enable-imenu))
  :custom
  (lsp-completion-provider . :none)
  (lsp-ui-doc-use-childframe . nil))

(leaf paredit
  :diminish paredit-mode
  :bind (:paredit-mode-map
         ("C-<right>" . right-word)
         ("C-<left>"  . left-word))
  :init
  (--each my/emacs-lisp-modes (add-hook it 'enable-paredit-mode)))

;; Scheme
(defun my-scheme-mode-setup ()
  "λ..."
  (paredit-mode t))

(leaf scheme
  :hook ((geiser-mode-hook . my/scheme-mode-hook)
         (scheme-mode-hook . my/scheme-mode-hook))
  :custom
  (geiser-active-implementations . '(guile racket)))

;; Haskell
(leaf haskell-mode
  :hook ((haskell-mode . turn-on-eldoc-mode)
         (haskell-mode . turn-on-haskell-indent)))

;; JavaScript
(leaf js2-mode
  :mode ("\\.js\\'"))

;; TypeScript
(defun my-setup-typescript ()
  "Setup function for TypeScript."
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(leaf typescript
  :mode ("\\.ts\\'" . typescript-mode)
  :hook ((typescript-mode . my-setup-typescript)))

;; text-mode
(defun my-text-mode-setup ()
  "Setup function for `text-mode'."
  (unless (eq major-mode 'html-mode)
    (setq line-spacing 5)))

(leaf diff-mode
  :mode ("/infection.log"))

(leaf text-mode
  :mode ("/LICENSE\\'")
  :hook ((text-mode . my-text-mode-setup)))

;; YAML
(leaf yaml-mode
  :mode ("/\\.gemrc\\'"))

;; Markdown Mode
(leaf markdown-mode
  ;;:mode ("\\.md\\'" . commonmark-gfm-mode)
  :config
  (eval-when-compile (defvar gfm-mode-map))
  (with-eval-after-load 'markdown-mode
    (require 'org-table)
    (add-hook 'markdown-mode-hook 'orgtbl-mode)
    (unbind-key "`" gfm-mode-map)
    (visual-line-mode nil)))

;; Magic Filetype
(leaf magic-filetype
  :init
  (magic-filetype-set-auto-mode 'ruby)
  (magic-filetype-enable-vim-filetype))

;;; Others:

;; Save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Recentf
(leaf recentf
  :custom
  (recentf-max-saved-items . 2000)
  (recentf-auto-cleanup . 'never)
  (recentf-exclude . '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.cache/"
                       "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\(\\.cask/\\|bookmarks\\)"
                       "/elpa/.*-autoloads\\.el\\'"  "/\\newsrc\\(\\.eld\\)?\\'"))
  :init
  (recentf-mode t)
  (run-with-idle-timer 30 t #'recentf-save-list))

;; Undo
(leaf undo-fu
  :bind (("C-_" . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)))

;; expand-region.el
(leaf expand-region
  :bind (("C-@" . er/expand-region)
         ("C-`" . er/contract-region)))

;; Annotate.el
(leaf annotate
  :bind (("M-@"   . annotate-annotate)
         ("C-M-@" . annotate-clear-annotations)))

;;; Tools:

;; Open junk file
(leaf open-junk-file
  :bind (("C-c j" . open-junk-file))
  :custom
  (open-junk-file-format . "~/junk/%Y/%m/%Y-%m-%d-%H%M%S-"))

;; restclient.el
(leaf restclient
  :mode ("\\.http\\'" . restclient-mode))

(leaf org-mode
  :init
  (custom-set-variables
   '(org-default-notes-file (concat org-directory "/capture.org")))
  (bind-key "C-c c" 'org-capture)
  (autoload 'ioslide:helper "ox-ioslide-helper.el" "Key menu for ioslide" t)
  :config
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)))))

;; Org-IO Slide
;;(require 'ox-ioslide-helper)

;; ElScreen
(leaf elscreen
  :init
  (custom-set-variables
   '(elscreen-prefix-key (kbd "C-z"))
   '(elscreen-display-tab nil)
   '(elscreen-tab-display-kill-screen nil)
   '(elscreen-tab-display-control nil))
  (bind-key "C-<tab>" 'elscreen-next)
  (bind-key "<C-iso-lefttab>" 'elscreen-previous)
  (elscreen-start)
  ;; El-Screeのウィンドウを一個つくる
  (elscreen-create))

(leaf ctrlf
  :diminish ctrlf-mode
  :init
  (ctrlf-mode +1))

(leaf rg
  :bind (("C-:" . rg)
         ("M-C-:" . rg-literal))
  :hook ((rg-mode . wgrep-rg-setup)))

;; direx
(leaf direx
  :bind (("M-C-\\" . direx-project:jump-to-project-root-other-window)
         ("M-C-¥"  . direx-project:jump-to-project-root-other-window)))

;; dired-k
(leaf dired-k
  :bind ((:dired-mode-map
          ("K" . dired-k)))
  :hook ((dired-initial-position-hook . dired-k)))

;; Visual
(leaf visual-regexp
  :bind (("M-%" . vr/query-replace)))

;; image-mode
(leaf image-mode
  :bind (:image-mode-map
         ("<wheel-up>"    . image-previous-line)
         ("<wheel-down>"  . image-next-line)
         ("<wheel-right>" . image-forward-hscroll)
         ("<wheel-left>"  . image-backward-hscroll)))

;; Yet another folding
(leaf yafolding
  :hook ((prog-mode-hook . yafolding-mode)))

;; vi-tilde-fringe
(leaf vi-tilde-fringe
  :diminish vi-tilde-fringe-mode
  :hook ((prog-mode-hook . vi-tilde-fringe-mode)))

(prog1 'goto-addr
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (add-hook 'text-mode-hook #'goto-address-mode))

(leaf smart-jump :ensure t)

;; multiple-cursors
;; http://qiita.com/ongaeshi/items/3521b814aa4bf162181d
(leaf multiple-cursors
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
(leaf which-key
  :diminish which-key-mode
  :custom
  (which-key-idle-delay . 1.5)
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))

;; smooth-scroll https://github.com/k-talo/smooth-scroll.el
(leaf smooth-scroll
  :diminish smooth-scroll-mode
  :custom
  (smooth-scroll/vscroll-step-size . 7)
  :init
  (require 'smooth-scroll)
  (smooth-scroll-mode t))

(leaf topsy :ensure t
  :hook '(prog-mode-hook))

(leaf puni :ensure t
  :hook '(php-mode-hook web-mode-hook))

;; crux
(leaf crux
  :init
  (leaf-keys
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
    ("C-DEL"   . crux-kill-line-backwards))))

(leaf vlf
  :custom
  (vlf-application . 'dont-ask)
  :init
  (require 'vlf-setup))

(leaf ov
  :init
  (autoload 'ov "ov.el" "Make an overlay from BEG to END.

If PROPERTIES are specified, set them for the created overlay."))

(leaf writeroom
  :custom
  (writeroom-fringes-outside-margins . t)
  (writeroom-maximize-window . nil)
  (writeroom-global-effects . '()))

(leaf nov
  :init
  (add-hook 'nov-post-html-render-hook #'writeroom-mode)
  (add-hook 'nov-post-html-render-hook #'my/disable-trailing-mode-hook))

;; TRAMP
(leaf tramp
  :init
  (with-eval-after-load 'tramp
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

(leaf diminish
  :init
  (safe-diminish "face-remap" 'buffer-face-mode)
  (safe-diminish "elisp-slime-nav" 'elisp-slime-nav-mode)
  (safe-diminish "flyspell" 'flyspell-mode)
  (safe-diminish "simple" 'auto-fill-function)
  (safe-diminish "subword" 'subword-mode)
  (safe-diminish "gcmh" 'gcmh-mode)
  (--each my/disable-trailing-modes
    (add-hook (intern (concat (symbol-name it) "-hook"))
              'my/disable-trailing-mode-hook)))
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
(leaf pandoc
  :init
  (pandoc-turn-on-advice-eww))

;; init-open-recentf
(when (eval-when-compile (file-directory-p "~/repo/emacs/init-open-recentf.el/"))
  (load "~/repo/emacs/init-open-recentf.el/init-open-recentf.el"))
(add-hook 'init-open-recentf-before-hook #'my/insert-tetosan)
(init-open-recentf)

;; Right Click
(leaf right-click-context
  :custom
  (right-click-context-mode-lighter . "")
  :init
  (right-click-context-mode 1))

;; Beacon — Never lose your cursor again
(leaf beacon
  :diminish beacon-mode
  :init
  (beacon-mode 1))

;; Eshell
(add-hook 'eshell-mode-hook 'eshell-fringe-status-mode)

(leaf highlight-indent-guides-method
  :diminish highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-method . 'character)
  (highlight-indent-guides-character . ?\|)
  (highlight-indent-guides-delay . 0.5))

(leaf google-translate
  :custom
  (google-translate-default-source-language . "en")
  (google-translate-default-target-language . "ja"))

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

;; keyfreq
(leaf keyfreq
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Auto deployment
(leaf copy-file-on-save
  :init
  (global-copy-file-on-save-mode 1))

(setq find-function-C-source-directory
      (eval-when-compile (f-expand "~/local/src/emacs/src")))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(gcmh-mode 1)

;; (message "Emacs finished loading (%d GCs)." gcs-done)

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
