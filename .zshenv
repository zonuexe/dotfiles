# -*- sh-shell: zsh; -*-

path=(
    ~/local/bin(N-/)
    ~/.local/bin(N-/)
    ~/.emacs.d/phpactor/vendor/bin(N-/)
    ~/.cargo/bin(N-/)
    ~/.config/composer/vendor/bin(N-/)
    ~/.deno/bin(N-/)
    ~/.bun/bin(N-/)
    ~/.grok/bin(N-/)
    ~/go/bin(N-/)
    "${HOME}/Library/Application Support/JetBrains/Toolbox/scripts"(N-/)
    ~/bin(N-/)
    /opt/homebrew/bin(N-/)
    /opt/homebrew/opt/gpatch/libexec/gnubin(N-/)
    /opt/homebrew/opt/ruby/bin(N-/)
    /opt/homebrew/opt/bison/bin(N-/)
    /opt/homebrew/opt/m4/bin(N-/)
    /opt/homebrew/opt/libiconv/bin(N-/)
    /opt/homebrew/opt/rustup/bin(N-/)
    /opt/homebrew/opt/openjdk/bin(N-/)
    /usr/local/sbin(N-/)
    /usr/local/bin(N-/)
    /usr/bin(N-/)
    /Applications/mpv.app/Contents/MacOS(N-/)
    /Applications/Wireshark.app/Contents/MacOS(N-/)
    $path
)

manpath=(
    /opt/homebrew/opt/coreutils/libexec/gnuman(N-/)
    $manpath
)
typeset -gxU manpath

export TEST_SERVER=${TEST_SERVER:-1}
export DENO_INSTALL="${HOME}/.deno"

if command -v vim >/dev/null 2>&1
then
   export EDITOR=vim
fi

if command -v less >/dev/null 2>&1
then
    export PAGER='less -R'
fi

typeset -U path cdpath fpath manpath

MY_ZSHENV=MY_ZSHENV
