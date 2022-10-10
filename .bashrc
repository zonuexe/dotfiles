# Fig pre block. Keep at the top of this file.
[[ -f "$HOME/.fig/shell/bashrc.pre.bash" ]] && builtin source "$HOME/.fig/shell/bashrc.pre.bash"
if $(which brew > /dev/null) && [ -f `brew --prefix`/etc/bash_completion ]
then
    . `brew --prefix`/etc/bash_completion
fi

alias tmxu=tmux
alias tumx=tmux

# Fig post block. Keep at the bottom of this file.
[[ -f "$HOME/.fig/shell/bashrc.post.bash" ]] && builtin source "$HOME/.fig/shell/bashrc.post.bash"
