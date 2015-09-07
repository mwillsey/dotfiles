
# source the private stuff that I don't want to put on github
command touch ~/.config/fish/private.fish
. ~/.config/fish/private.fish

# os specific
switch (uname)
    case Darwin
        alias vim "mvim -v"
        set -x EDITOR "mvim -v"
    case *
        set -x EDITOR "vim"
end

# for tmux/screen specific things
switch $TERM
    case "screen*"
        set -g __fish_in_screen true
end
    

# easy aliases
alias l "ls -lah"

