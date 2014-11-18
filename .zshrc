bindkey -e
autoload -U compinit; compinit

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

autoload -U compinit && compinit
zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

if [ -f /Applications/MacVim.app/Contents/MacOS/Vim ]
then
  alias vim=/Applications/MacVim.app/Contents/MacOS/Vim
fi

