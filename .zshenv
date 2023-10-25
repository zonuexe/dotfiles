# -*- sh-shell: zsh; -*-

path=(
    ~/local/bin(N-/)
    ~/.cargo/bin(N-/)
    ~/.rbenv/bin(N-/)
    ~/.phpenv/bin(N-/)
    ~/.rbenv/shims(N-/)
    ~/.phpenv/shims(N-/)
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
    /usr/local/opt/php@7.4/bin(N-/)
    /usr/local/bin/python3(N-/)
    /usr/local/opt/ruby/bin(N-/)
    /usr/local/opt/go/libexec/bin(N-/)
    /usr/local/opt/imagemagick@6/bin(N-/)
    /usr/local/opt/bison/bin(N-/)
    /usr/local/opt/curl/bin(N-/)
    /usr/local/opt/libiconv/bin(N-/)
    /usr/local/sbin(N-/)
    /usr/local/bin(N-/)
    /usr/bin(N-/)
    /usr/local/opt/coreutils/libexec/gnubin(N-/)
    /usr/local/opt/go/libexec/bin(N-/)
    /usr/local/heroku/bin(N-/)
    /Applications/EasyWine64.app/Contents/Resources/wine/bin(N-/)
    /Applications/mpv.app/Contents/MacOS(N-/)
    /Applications/Wireshark.app/Contents/MacOS(N-/)
    $path
)

manpath=(
    /usr/local/opt/coreutils/libexec/gnuman(N-/)
    $manpath
)
typeset -gxU manpath

export NVM_DIR=~/.nvm
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

if which rbenv > /dev/null
then
    eval "$(rbenv init -)"
fi

if [ -d /usr/local/opt/openssl ]
then
    export CPPFLAGS="-I/usr/local/opt/openssl/include"
fi

[ -z "$ld_library_path" ] && typeset -xT LD_LIBRARY_PATH ld_library_path
[ -z "$include" ] && (typeset -xT INCLUDE include; typeset -U include)
typeset -U path cdpath fpath manpath ld_library_path

export LD_LIBRARY_PATH=$HOME/local/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/local/lib/pkgconfig/

MY_ZSHENV=MY_ZSHENV
