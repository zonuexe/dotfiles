if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

PATH=$HOME/local/bin:$HOME/.composer/vendor/bin:$HOME/.linuxbrew/bin:$PATH

[ -e "${HOME}/.iterm2_shell_integration.bash" ] && . "${HOME}/.iterm2_shell_integration.bash"
