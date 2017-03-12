# Max's .bashrc on bicycle

include () { 
    for file in $@; do 
        [ -f "$file" ] && source "$file"
    done
}

include /etc/bashrc
include ~/.bashrc.local
include ~/fzf.bash
include /etc/bash_completion.d/git

# A two-line colored Bash prompt (PS1) with Git branch and a line decoration
# which adjusts automatically to the width of the terminal.
# Recognizes and shows Git and SVN branch/revision.
# https://gist.github.com/mkottman/1936195

RESET="\[\033[0m\]"
RED="\[\033[0;31m\]"
GREEN="\[\033[01;32m\]"
BLUE="\[\033[01;34m\]"
YELLOW="\[\033[0;33m\]"

PS_LINE=`printf -- ' %.0s' {1..200}`
function parse_git_branch {
  PS_BRANCH=''
  PS_FILL=${PS_LINE:0:$COLUMNS}
  if [ -d .svn ]; then
    PS_BRANCH="(svn r$(svn info|awk '/Revision/{print $2}'))"
    return
  fi
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  PS_BRANCH="(git ${ref#refs/heads/}) "
}
PROMPT_COMMAND=parse_git_branch
PS_INFO="$GREEN\u@\h$RESET:$BLUE\w"

PS_TIME="\[\033[\$((COLUMNS-10))G\] $RED[\t]"
export PS1="\${PS_FILL}\[\033[0G\]${PS_INFO} ${PS_GIT}${PS_TIME}\n${RESET}\$ "


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
export PYTHONPATH="$LOCAL/lib/python2.7/site-packages:$PYTHONPATH"

export EDITOR=vim

# aliases and shortcuts
alias l='ls -al'
