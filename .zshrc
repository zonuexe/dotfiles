# -*- sh-shell: zsh; -*-

[[ "$MY_ZSHENV" = MY_ZSHENV ]] && . ~/.zshenv

bindkey -e

setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups

setopt extended_glob
setopt noautoremoveslash
setopt correct
setopt list_packed
setopt nolistbeep

setopt auto_param_keys
setopt auto_param_slash
setopt complete_aliases
setopt numeric_glob_sort

setopt equals
setopt magic_equal_subst

HISTFILE=$HOME/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
setopt hist_ignore_space
setopt share_history
setopt extended_history

zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

case "${OSTYPE}" in
freebsd*|darwin*)
    if command -v ls | grep -q gnubin 2>/dev/null
    then
        ls=coreutils
    else
        ls=bsdls
    fi
    ;;
linux*)
    ls=coreutils
    ;;
esac

# 環境に合わせて出来るだけ好みのls出力にする
# http://qiita.com/kawaz/items/a44f42bd5099efaab431
if [ "$ls" = "bsdls" ]; then
   alias ls='ls -G -w'
elif ls -d -N >/dev/null 2>&1; then
  # 最近の gnubin/ls が勝手にファイル名をクオート出力してくるのを抑止
  alias ls='ls --color --time-style +%Y-%m-%d\ %H:%M:%S.%3N -N'
elif ls -d --time-style +%Y-%m-%d\ %H:%M:%S.%3N >/dev/null 2>&1; then
  # デフォの時間表示は見難いのでISOぽくする(full-iso,long-iso,isoは帯に短し襷に長しなのでカスタム)
  alias ls='ls --color --time-style +%Y-%m-%d\ %H:%M:%S.%3N'
elif ls -d --color >/dev/null 2>&1; then
  # せめて色だけでも…
  alias ls='ls --color'
fi

autoload -Uz colors && colors

fpath=(
    $HOME/local/zsh/completions(N-/)
    $HOME/local/zsh/functions(N-/)
    $fpath
)

case ${UID} in
0)
    PROMPT="%F{blue}%n%f %F{blue}#%f "
    PROMPT2="%B%F{blue}%_#%f%b "
    SPROMPT="%B%F{blue}%r is correct? [n,y,a,e]:%f%b "
    RPROMPT="%F{blue}[%/]%f"
    ;;
*)
    PROMPT="%n %F{blue}%%%f "
    PROMPT2="%B%F{blue}%_#%f%b "
    SPROMPT="%B%F{blue}%r is correct? [n,y,a,e]:%f%b "
    RPROMPT="%F{blue}[%/]%f"
    ;;
esac

case "${TERM}" in
xterm*)
    export LSCOLORS=exfxcxdxbxegedabagacad
    export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
    zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
    ;;
esac

autoload -Uz add-zsh-hook
set-window-title() {
    print -Pn "\e]0;%n@%m:%~\a"
}
case "${TERM}" in
xterm*|tmux*|screen*|alacritty|wezterm|ghostty|kitty)
    add-zsh-hook precmd set-window-title
    ;;
esac

if [[ -f /Applications/Emacs.app/Contents/MacOS/Emacs ]]
then
    alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
fi

if [[ -f /opt/homebrew/bin/brew ]]
then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

alias あ=rg
alias s=ls
alias be="bundle exec"
alias tmxu=tmux
alias tumx=tmux

tree(){
    command tree -C $* | less -R
}


jqless(){ cat ${1:--} | jq ${2:-.} -C | less -R }

if command -v xsel >/dev/null 2>&1
then
    alias pbcopy='xsel --clipboard --input'
    alias pbpaste='xsel --clipboard --output'
fi

if ! command -v nproc >/dev/null 2>&1
then
    alias nproc='sysctl -n hw.ncpu'
fi

export RAILS_ENV=development
export MICRO_TRUECOLOR=1

export PHAN_BIN=$HOME/.config/composer/vendor/bin/phan

phpstan() {
    if [[ -f 'vendor/bin/phpstan' ]] ; then
        'vendor/bin/phpstan' --memory-limit=2G "$@"
    elif [[ -f "$(composer config bin-dir)/phpstan" ]] ; then
        "$(composer config bin-dir)/phpstan" --memory-limit=2G "$@"
    else
        command phpstan --memory-limit=2G "$@"
    fi
}

phptags(){
    ctags -e --php-types=c+i+d+f $(git ls-files '*.php' | grep -v __snapshots__)
}

vld () {
    php -dvld.active=1 -dvld.execute=0 "$@" 2>&1
}

cdiff() {
    diff -u --color=always "$@" | less -R
}

if command -v zprof >/dev/null 2>&1 ;then
    zprof | less
fi

# --- ツール初期化（fpath を触るもの → 最後に compinit 一回） ---

if command -v mise >/dev/null 2>&1
then
    eval "$(mise activate zsh)"
fi

if [[ -o interactive ]] && command -v zoxide >/dev/null 2>&1
then
    if [[ -n ${CLAUDECODE:-} ]]
    then
        # Claude Code は cd を独自に扱うので置き換えない
        eval "$(zoxide init zsh)"
    else
        eval "$(zoxide init zsh --cmd cd)"
    fi
fi

# Nix
if [[ -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
  . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
elif [[ -e /nix/var/nix/profiles/default/etc/profile.d/nix.sh ]]; then
  . /nix/var/nix/profiles/default/etc/profile.d/nix.sh
fi

[[ -n $EAT_SHELL_INTEGRATION_DIR ]] && . "$EAT_SHELL_INTEGRATION_DIR/zsh"

# bun（bin は .zshenv の path 配列で設定済み）
export BUN_INSTALL=${BUN_INSTALL:-$HOME/.bun}
[[ -s $BUN_INSTALL/_bun ]] && source "$BUN_INSTALL/_bun"

# grok（bin は .zshenv の path 配列で設定済み）
fpath=($HOME/.grok/completions/zsh(N-/) $fpath)

autoload -Uz compinit
# dumpが24時間より古い時だけ再生成（起動高速化と新規補完の検出を両立）
if [[ -n ${ZDOTDIR:-$HOME}/.zcompdump(#qN.mh+24) ]]; then
  compinit
else
  compinit -C
fi

# pnpm
export PNPM_HOME="${HOME}/Library/pnpm"
[[ -d $PNPM_HOME ]] && path=($PNPM_HOME $path)
# pnpm end
