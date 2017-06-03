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

# this is kind of a bad thing
# to avoid actually having local perl modules, I just point PERL5LIB to the
# stow install so it can find its own perl module
export PERL5LIB="$STOW_DIR/stow/share/perl5:$PERL5LIB"
# export PYTHONPATH="$LOCAL/lib/python2.7/site-packages:$PYTHONPATH"
export PYTHONUSERBASE=$LOCAL

# lazily load python virtualenv wrapper
# http://virtualenvwrapper.readthedocs.io/en/latest/install.html
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=  # don't use project management
export VIRTUALENVWRAPPER_SCRIPT=/usr/local/bin/virtualenvwrapper.sh
source /usr/local/bin/virtualenvwrapper_lazy.sh

export EDITOR=vim
export GLOBIGNORE=".:.."

# aliases and shortcuts
alias l='ls -al'
alias ec='emacsclient -t -a vim'
alias xdg-open='client open'
