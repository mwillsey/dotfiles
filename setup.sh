#!/bin/sh

BASEDIR=~/dotfiles
# sets up the symlinks

# vim
FILES="
.vimrc
.emacs.d
.zshrc
.tmux.conf
"

for file in $FILES
do
    if [ -e ~/$file ]
    then
        echo "~/$file exists, not creating link..."
    else
        ln -s $BASEDIR/$file ~/$file
        echo "created link ~/$file -> $BASEDIR/$file"
    fi
done
