# Max's .bashrc on bicycle

function include () {
    for file in $@; do
        [ -f "$file" ] && source "$file"
    done
}

include /etc/bashrc
include ~/.bashrc.local
include ~/fzf.bash
include /etc/bash_completion.d/git

RESET="\[\033[0m\]"
RED="\[\033[0;31m\]"
GREEN="\[\033[01;32m\]"
BLUE="\[\033[01;34m\]"
YELLOW="\[\033[0;33m\]"

export PS1="${GREEN}\u@\h${RESET}:${BLUE}\w${RESET}\n\$ "


# Environment variables

export LOCAL=$HOME/local
export PREFIX=$LOCAL

export PATH="$LOCAL/bin:$PATH"
export MANPATH="$LOCAL/share/man:$MANPATH"
export LD_LIBRARY_PATH="$LOCAL/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="$LOCAL/lib/pkgconfig:$PKG_CONFIG_PATH"
export STOW_DIR="$LOCAL/stow"

# this is kind of a bad thing
# to avoid actually having local perl modules, I just point PERL5LIB to the
# stow install so it can find its own perl module
export PERL5LIB="$STOW_DIR/stow/share/perl5:$PERL5LIB"
# export PYTHONPATH="$LOCAL/lib/python2.7/site-packages:$PYTHONPATH"
export PYTHONUSERBASE=$LOCAL

export EDITOR=vim
export GLOBIGNORE=".:.."

# aliases and shortcuts
alias l='ls -al'
alias ec='emacsclient -t -a vim'
alias xdg-open='client open'
