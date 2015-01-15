# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="flazz"

# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration
export EDITOR='mvim -v'
alias vim='mvim -v'
alias vi='mvim -v'

# OPAM configuration
. /Users/mwillsey/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
