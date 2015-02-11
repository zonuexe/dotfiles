path=(
    ~/.rbenv/bin(N-/)
    ~/.rbenv/shims(N-/)
    ~/.composer/vendor/bin(N-/)
    ~/local/bin(N-/)
    ~/.cask/bin(N-/)
    ~/.linuxbrew/bin(N-/)
    /usr/local/bin(N-/)
    /usr/bin(N-/)
    /usr/local/opt/coreutils/libexec/gnubin(N-/)
    $path
)

manpath=(
    /usr/local/opt/coreutils/libexec/gnuman(N-/)
    $manpath
)
typeset -gxU manpath

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
    
fi

if type brew > /dev/null
then
    export PYTHONPATH=`brew --prefix`/lib/python2.7/site-packages:$PYTHONPATH
fi

if [ -f $HOME/.phpbrew/bashrc ]
then
    . $HOME/.phpbrew/bashrc
fi

export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/.go

if type launchctl > /dev/null
then
    launchctl setenv GOROOT $GOROOT
    launchctl setenv GOPATH $GOPATH
fi

if [ -d /usr/local/opt/openssl ]
then
    export CPPFLAGS="-I/usr/local/opt/openssl/include"
fi

[ -z "$ld_library_path" ] && typeset -xT LD_LIBRARY_PATH ld_library_path
[ -z "$include" ] && typeset -xT INCLUDE include
typeset -U path cdpath fpath manpath ld_library_path include

ld_library_path=(
    $HOME/.linuxbrew/lib(N-/)
    /usr/local/opt/openssl/lib(N-/)
    /usr/local/opt/sqlite/lib(N-/)
    /usr/local/opt/icu4c/lib(N-/)
    $ld_library_path
)
#include=(${HOME}/include(N-/) $include)
