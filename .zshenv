path=(
    ~/.rbenv/bin(N-/)
    ~/.rbenv/shims(N-/)
    ~/.composer/vendor/bin(N-/)
    ~/local/bin(N-/)
    ~/.cask/bin(N-/)
    /usr/local/bin(N-/)
    /usr/bin(N-/)
    $path
)

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
