## max's .zshrc

# source local config if present
if [[ -f ~/.zshrc.local ]]; then
    echo "I found a sourced a '.zshrc.local' file!"
    source ~/.zshrc.local
fi

# basic config 
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob
bindkey -e
KEYTIMEOUT=1

# ls colors
export LSCOLORS="GxFxCxDxBxegedabagaced"
export LS_COLORS="di=1;36:ln=1;35:so=1;32:pi=1;33:ex=1;\
31:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=34;43:"

# aliases
case `uname` in
    Darwin) 
        alias ls='ls -FG' 
        alias vim='mvim -v'
        ;;
    Linux)  
        alias ls='ls -F --color=auto' 
        ;;
esac
alias l='ls -lah'

# enviroment variables
export EDITOR=vim

# completion
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
# blank to prefer exact matching first
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z} r:|[._-]=* r:|=* l:|=* r:|=*'
zstyle ':completion:*' insert-tab false
zstyle ':completion:*' menu select
# zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit
setopt completeinword
compinit

# initialize colors
autoload -U colors
colors

# start at home
cd $HOME

# set prompt
function () {

    local host="%m"
    if [[ $USER != "mwillsey" && $USER != "hwillsey" ]]; then
        host="%{$fg_bold[red]%}%u@" # unexpected user
    fi

    local dir="%{$fg_bold[green]%}%(3~,…/,)%2~"

    local char_color="%(?,$fg_bold[green],$fg_bold[red])"
    local char="%{$char_color%}➜%{$reset_color%}"

    PROMPT="$host $dir $char "
}

# dont exit if in tmux or screen
if [[ $TERM =~ "screen" ]]; then
    setopt ignoreeof
fi

# welcome
echo ""
echo "Hello $bold_color$USER$reset_color!"
echo "Welcome to $bold_color$HOST$reset_color!"
echo "It's $(date +%c)."
echo ""

# edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey "^x^e" edit-command-line

