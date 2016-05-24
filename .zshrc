## max's .zshrc

# source local config if present
if [[ -f ~/.zshrc.local ]]; then
    echo "I found a sourced a '.zshrc.local' file!"
    source ~/.zshrc.local
fi

# source nix if present
if [ -f ~/.nix-profile/etc/profile.d/nix.sh ]; then 
    . ~/.nix-profile/etc/profile.d/nix.sh;
fi 

# function to ssh and tmux in (detaching all others)
function ssht () {ssh -t $@ "tmux attach -d || tmux new";}

# basic config 
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob
bindkey -e
KEYTIMEOUT=1

export LSCOLORS="GxFxCxDxBxegedabagaced"
export LS_COLORS="di=1;36:ln=1;35:so=1;32:pi=1;33:ex=1;\
    -31:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=34;43:"

# export LSCOLORS="Axaxaxaxaxahahahahahah"
# export LS_COLORS="di=1;0;40:ln=0;40:so=0;40:pi=0;40:ex=0;40:\
#     bd=0;47:cd=0;47:su=0;47:sg=0;47:tw=0;47:ow=0;47:"

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
export RUST_SRC_PATH=~/rust/src

# completion
# blank to prefer exact matching first
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z} r:|[._-]=* r:|=* l:|=* r:|=*'
zstyle ':completion:*' insert-tab false
zstyle ':completion:*' menu select
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
    # local 
    # if [[ $USER != "mwillsey" && $USER != "hwillsey" ]]; then
    #     local char="$"
    # fi

    local dir="%(3~,…/,)%2~"

    prompt="[$host %~]%# "
    PROMPT="%{$fg_bold[white]%}$prompt%{$reset_color%}"
}

precmd() { print -rP "%{$fg_bold[white]%}[%n@%m %~]%{$reset_color%}" }
export PROMPT="%(!.#.>) "

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

