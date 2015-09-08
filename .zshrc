## max's .zshrc

# basic config 
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob
bindkey -e
KEYTIMEOUT=1

# aliases
if [[ $(uname) == Linux ]]; then
    alias ls='ls --color=auto'
fi
alias l='ls -lah'

# enviroment variables
export EDITOR=vim

# completion
zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit
compinit

# initialize colors
autoload -U colors
colors

# simple prompt
PROMPT="> "

# dont exit if in tmux or screen
if [[ $TERM =~ "screen" ]]; then
    setopt ignoreeof
fi

# status function
function my-status() {
    local you="$USER@$HOST"
    local dir="$fg_bold[green]${$(pwd)/#$HOME/~}$reset_color"
    echo -n "$you $dir"
}

# welcome
echo ""
echo "Hello $bold_color$USER$reset_color!"
echo "Welcome to $bold_color$HOST$reset_color!"
echo "It's $(date +%c)."
echo ""

# edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey "\e" edit-command-line

# empty command runs my-status
function my-accept-line() {
    zle accept-line
    if [[ -z $BUFFER ]]; then
        echo
        my-status
    fi
}
zle -N my-accept-line
bindkey "^M" my-accept-line

# changing dirs runs my-status
function chpwd() {
    my-status
    echo
}

