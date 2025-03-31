# -*- sh-shell: zsh; -*-

path=(
    ~/local/bin(N-/)
    ~/.local/bin(N-/)
    ~/.emacs.d/phpactor/vendor/bin(N-/)
    ~/.cargo/bin(N-/)
    ~/.symfony/bin
    ~/.config/composer/vendor/bin(N-/)
    ~/.composer/vendor/bin(N-/)
    ~/.cask/bin(N-/)
    ~/.deno/bin(N-/)
    ~/.bun/bin(N-/)
    ~/.phpkg(N-/)
    ~/bin(N-/)
    /opt/homebrew/bin(N-/)
    /opt/homebrew/opt/ruby/bin(N-/)
    /opt/homebrew/opt/bison/bin(N-/)
    /opt/homebrew/opt/m4/bin(N-/)
    /opt/homebrew/opt/libiconv/bin(N-/)
    /usr/local/sbin(N-/)
    /usr/local/bin(N-/)
    /usr/bin(N-/)
    /Applications/mpv.app/Contents/MacOS(N-/)
    /Applications/Wireshark.app/Contents/MacOS(N-/)
    $path
)

manpath=(
    /usr/local/opt/coreutils/libexec/gnuman(N-/)
    $manpath
)
typeset -gxU manpath

export GOPATH=$HOME/repo/go
export XML_CATALOG_FILES=/usr/local/etc/xml/catalog
export TEST_SERVER=${TEST_SERVER:-1}
export DENO_INSTALL="${HOME}/.deno"
export CLOUDSDK_PYTHON_SITEPACKAGES=1

if which vim > /dev/null > /dev/null
then
   export EDITOR=vim
fi

if which lv > /dev/null
then
    export PAGER='less -R'
fi

[ -z "$ld_library_path" ] && typeset -xT LD_LIBRARY_PATH ld_library_path
[ -z "$include" ] && (typeset -xT INCLUDE include; typeset -U include)
typeset -U path cdpath fpath manpath ld_library_path

export LD_LIBRARY_PATH=$HOME/local/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/local/lib/pkgconfig/

MY_ZSHENV=MY_ZSHENV
