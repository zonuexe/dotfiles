;;; Code:

(require 'package)
(require 'quelpa (locate-user-emacs-file "site-lisp/quelpa/quelpa"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (featurep 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))


(require 'use-package)

(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

(quelpa '0xc)
(quelpa '2048-game)
(quelpa 'aa-edit-mode)
(quelpa 'ac-geiser)
(quelpa 'ac-html)
(quelpa 'ac-ispell)
(quelpa 'actionscript-mode)
(quelpa 'adoc-mode)
(quelpa 'ag)
(quelpa 'all-the-icons-dired)
(quelpa 'anaphora)
(quelpa 'annotate)
(quelpa 'aozora-view)
(quelpa 'apache-mode)
(quelpa 'atomic-chrome)
(quelpa 'auto-complete)
(quelpa 'auto-minor-mode)
(quelpa 'auto-read-only)
(quelpa 'beacon)
(quelpa 'benchmark-init)
(quelpa 'bind-key)
(quelpa 'bm)
(quelpa 'brainfuck-mode)
(quelpa 'calfw)
;;(quelpa 'calfw-git" :git 'https://gist.github.com/d77d9669440f3336bb9d.git)
;;(quelpa 'calfw-syobocal" :git 'https://gist.github.com/1fd257fc1e8907d4d92e.git" :files ("*.el))
(quelpa 'cedit)
(quelpa 'cmake-mode)
(quelpa 'codic)
(quelpa 'coffee-mode)
(quelpa 'color-moccur)
(quelpa 'commonmark '(:fetcher github :repo "zonuexe/CommonMark.el"))
(quelpa 'composer)
(quelpa 'copy-file-on-save)
(quelpa 'copyit)
(quelpa 'copyit-pandoc)
(quelpa 'counsel)
(quelpa 'counsel-osx-app)
(quelpa 'cov)
(quelpa 'crux)
(quelpa 'csv-mode)
(quelpa 'dash)
(quelpa 'dash-at-point)
(quelpa 'datetime-format)
(quelpa 'diminish)
(quelpa 'dired-k)
(quelpa 'dired-toggle '(:fetcher github :repo "zonuexe/dired-toggle"))
(quelpa 'direx)
(quelpa 'dmacro '(:fetcher github :repo "zonuexe/dmacro"))
(quelpa 'dockerfile-mode)
(quelpa 'drag-stuff)
(quelpa 'ecukes)
(quelpa 'edbi)
(quelpa 'edbi-sqlite)
(quelpa 'ede-php-autoload)
(quelpa 'edit-server)
(quelpa 'editorconfig)
(quelpa 'elisp-slime-nav)
(quelpa 'elixir-mode)
(quelpa 'elscreen)
(quelpa 'emmet-mode)
(quelpa 'emms-player-mpv)
(quelpa 'emms-player-mpv-jp-radios)
(quelpa 'emoji-cheat-sheet-plus)
(quelpa 'emoji-fontset)
(quelpa 'enh-ruby-mode)
(quelpa 'enlive)
(quelpa 'ensime)
(quelpa 'esa)
(quelpa 'eshell-fringe-status)
(quelpa 'ess)
(quelpa 'evalator)
(quelpa 'expand-region)
(quelpa 'f)
(quelpa 'flycheck)
(quelpa 'flycheck-cask)
(quelpa 'flycheck-package)
(quelpa 'flycheck-rust)
(quelpa 'font-lock-studio)
(quelpa 'font-utils)
(quelpa 'format-sql)
(quelpa 'fsharp-mode)
(quelpa 'geben)
(quelpa 'geiser)
(quelpa 'gist)
(quelpa 'gitignore-mode)
(quelpa 'gnugo)
(quelpa 'go-mode)
(quelpa 'google-translate)
(quelpa 'grapnel)
(quelpa 'habitica)
(quelpa 'hacker-typer)
(quelpa 'hamburger-menu)
(quelpa 'haml-mode)
(quelpa 'haskell-mode)
(quelpa 'helm)
(quelpa 'helm-ag)
(quelpa 'helm-dash)
(quelpa 'helm-descbinds)
(quelpa 'helm-emms)
(quelpa 'helm-git-files)
(quelpa 'helm-img-tiqav)
(quelpa 'helm-migemo)
(quelpa 'helm-projectile)
(quelpa 'helm-smex)
(quelpa 'helm-swoop)
(quelpa 'helm-themes)
(quelpa 'hierarchy)
(quelpa 'ht)
(quelpa 'htmlize)
(quelpa 'http)
(quelpa 'hydra)
(quelpa 'idle-highlight-mode)
(quelpa 'ido-cr+)
(quelpa 'ido-vertical-mode)
(quelpa 'indent-guide)
(quelpa 'inf-ruby)
(quelpa 'init-open-recentf)
(quelpa 'ivs-edit)
(quelpa 'ivy)
(quelpa 'jade-mode)
(quelpa 'jetbrains)
(quelpa 'js2-mode)
(quelpa 'json-mode)
(quelpa 'key-chord)
(quelpa 'key-combo)
(quelpa 'keyfreq)
(quelpa 'less-css-mode)
(quelpa 'lispxmp)
(quelpa 'load-theme-buffer-local)
(quelpa 'logalimacs)
(quelpa 'logview)
(quelpa 'lsp-mode)
(quelpa 'lua-mode)
(quelpa 'macrostep)
(quelpa 'magic-filetype)
(quelpa 'magit)
(quelpa 'magit-find-file)
(quelpa 'markdown-mode)
(quelpa 'markdown-preview-eww)
(quelpa 'maruo-macro-mode)
(quelpa 'minesweeper)
(quelpa 'mode-test '(:fetcher github :repo "emacs-php/mode-test" :files ("mode-test.el")))
(quelpa 'multiple-cursors)
(quelpa 'nameless)
(quelpa 'navi2ch)
(quelpa 'neon-mode)
(quelpa 'neotree)
(quelpa 'nginx-mode)
(quelpa 'nim-mode)
(quelpa 'nlinum)
(quelpa 'nodejs-repl)
(quelpa 'nord-theme)
(quelpa 'nyan-mode)
(quelpa 'ob-ipython)
(quelpa 'ob-nim)
(quelpa 'open-junk-file)
(quelpa 'org-ac)
(quelpa 'org-rtm)
(quelpa 'org-table-sticky-header)
(quelpa 'org-trello)
(quelpa 'osx-lib)
(quelpa 'ov)
(quelpa 'ox-ioslide)
(quelpa 'pacmacs)
(quelpa 'page-break-lines)
(quelpa 'pandoc)
(quelpa 'paredit)
(quelpa 'pcap-mode '(:fetcher github :repo "orgcandman/pcap-mode"))
(quelpa 'pdf-tools)
(quelpa 'peep-dired)
(quelpa 'phan)
(quelpa 'php-mode)
(quelpa 'phpstan '(:fetcher github :repo "emacs-php/phpstan.el"))
(quelpa 'php-util '(:fetcher github :repo "zonuexe/php-util.el"))
;;(quelpa 'php7-mode '(:fetcher github :repo "emacs-php/Php7mode"))
(quelpa 'phpstan '(:fetcher github :repo "emacs-php/phpstan.el"))
(quelpa 'phpunit '(:fetcher github :repo "nlamirault/phpunit.el" :branch "develop"))
(quelpa 'presentation '(:fetcher github :repo "zonuexe/emacs-presentation-mode"))
(quelpa 'pixiv-novel-mode)
(quelpa 'pomodoro)
(quelpa 'ponylang-mode)
(quelpa 'popwin)
(quelpa 'powershell)
(quelpa 'prodigy)
(quelpa 'projectile)
(quelpa 'projectile-rails)
(quelpa 'psysh)
(quelpa 'qiita)
(quelpa 'quickrun)
(quelpa 'rainbow-mode)
(quelpa 'rebecca-theme)
(quelpa 'recentf-ext)
(quelpa 'region-convert)
(quelpa 'request-deferred)
(quelpa 'restclient)
(quelpa 'review-mode)
(quelpa 'right-click-context '(:repo "zonuexe/right-click-context" :fetcher github))
(quelpa 'ripgrep)
(quelpa 'robe)
(quelpa 'robots-txt-mode)
(quelpa 'rust-mode)
(quelpa 's)
(quelpa 'scss-mode)
(quelpa 'sequential-command)
(quelpa 'shx)
(quelpa 'sl)
(quelpa 'slim-mode)
;;(quelpa 'sly)
(quelpa 'smart-newline)
(quelpa 'smartchr '(:repo "imakado/emacs-smartchr" :fetcher github))
(quelpa 'smartparens)
(quelpa 'smartrep)
(quelpa 'smex)
(quelpa 'smooth-scroll)
(quelpa 'sokoban)
;;(quelpa 'sql-drill)
(quelpa 'ssh-config-mode)
(quelpa 'stylus-mode)
(quelpa 'sudoku)
(quelpa 'suggest)
(quelpa 'swoop)
(quelpa 'test-simple)
(quelpa 'threes)
(quelpa 'thrift)
(quelpa 'timecop)
(quelpa 'toml)
(quelpa 'toml-mode)
(quelpa 'treemacs)
(quelpa 'tss)
(quelpa 'ucs-utils)
(quelpa 'undo-tree)
(quelpa 'untitled-new-buffer)
(quelpa 'use-package)
(quelpa 'vagrant-tramp)
(quelpa 'vi-tilde-fringe)
(quelpa 'vimrc-mode)
(quelpa 'visual-regexp)
(quelpa 'volatile-highlights)
(quelpa 'w3m)
(quelpa 'wandbox)
(quelpa 'wc-mode)
(quelpa 'web-mode)
(quelpa 'wgrep)
(quelpa 'wgrep-ag)
(quelpa 'which-key)
(quelpa 'writeroom-mode)
(quelpa 'xterm-keybinder '(:fetcher github :repo "yuutayamada/xterm-keybinder-el"))
(quelpa 'yafolding)
(quelpa 'yaml-mode)
(quelpa 'yard-mode)
(quelpa 'yasnippet)
(quelpa 'yasnippet-snippets)
(quelpa 'zenity-color-picker)

;;; my-packages.el ends here
