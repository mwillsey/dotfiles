# Max's .bashrc on bicycle

function include () {
    for file in $@; do
        [ -f "$file" ] && source "$file"
    done
}

include /etc/bashrc
include ~/.bashrc.local
include ~/fzf.bash
include /usr/local/etc/bash_completion

RESET="\[\033[0m\]"
RED="\[\033[0;31m\]"
GREEN="\[\033[01;32m\]"
BLUE="\[\033[01;34m\]"
YELLOW="\[\033[0;33m\]"

export PS1="${GREEN}\u@\h${RESET}:${BLUE}\w${RESET}\n\$ "

# this is kind of a bad thing
# to avoid actually having local perl modules, I just point PERL5LIB to the
# stow install so it can find its own perl module
export PERL5LIB="$STOW_DIR/stow/share/perl5:$PERL5LIB"

# python stuff pew, pipenv
export PYTHONUSERBASE=$LOCAL
export WORKON_HOME=$HOME/.virtualenvs

export EDITOR=vim
export GLOBIGNORE=".:.."

# aliases and shortcuts
alias l='ls -al'
alias ec='emacsclient -t -a vim'
alias xdg-open='client open'
