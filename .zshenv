path=(
    ~/.rbenv/bin(N-/)
    ~/.rbenv/shims(N-/)
    ~/.composer/vendor/bin(N-/)
    ~/local/bin(N-/)
    ~/.cask/bin(N-/)
    ~/.linuxbrew/sbin(N-/)
    ~/.linuxbrew/bin(N-/)
    ~/.linuxbrew/sbin(N-/)
    #$(brew --prefix homebrew/php/php56)/bin(N-/)
    /usr/local/sbin(N-/)
    /usr/local/bin(N-/)
    /usr/bin(N-/)
    /usr/local/opt/coreutils/libexec/gnubin(N-/)
    /usr/local/heroku/bin(N-/)
    /Applications/mpv.app/Contents/MacOS(N-/)
    $path
)

manpath=(
    /usr/local/opt/coreutils/libexec/gnuman(N-/)
    $manpath
)
typeset -gxU manpath

export NVM_DIR=~/.nvm
source $(brew --prefix nvm)/nvm.sh

if type vim > /dev/null > /dev/null
then
   export EDITOR=vim
fi

if type lv > /dev/null
then
    export PAGER='lv -c'
fi

if which rbenv > /dev/null
then
    eval "$(rbenv init -)"
fi


if [ -f $HOME/.phpbrew/bashrc ]
then
    . $HOME/.phpbrew/bashrc
fi

export GOPATH=$HOME
export SHELLY_HOME=$HOME/.shelly;
[ -s "$SHELLY_HOME/lib/shelly/init.sh" ] && . "$SHELLY_HOME/lib/shelly/init.sh"

if type launchctl > /dev/null
then
    launchctl setenv GOPATH $GOPATH
fi

if [ -d /usr/local/opt/openssl ]
then
    export CPPFLAGS="-I/usr/local/opt/openssl/include"
fi

[ -z "$ld_library_path" ] && typeset -xT LD_LIBRARY_PATH ld_library_path
[ -z "$include" ] && (typeset -xT INCLUDE include; typeset -U include)
typeset -U path cdpath fpath manpath ld_library_path

ld_library_path=(
    $HOME/.linuxbrew/lib(N-/)
    /usr/local/opt/curl/lib(N-/)
    /usr/local/opt/openssl/lib(N-/)
    /usr/local/opt/sqlite/lib(N-/)
    /usr/local/opt/icu4c/lib(N-/)
    /usr/local/opt/gettext/lib(N-/)
    $ld_library_path
)
#include=(${HOME}/include(N-/) $include)

MY_ZSHENV=MY_ZSHENV
