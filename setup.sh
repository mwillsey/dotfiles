#!/bin/sh

BASEDIR=$(dirname $0)
# sets up the symlinks

# vim
FILES=( .vim .vimrc .emacs.d .tmux.conf)

for file in ${FILES[@]}
do
    if [ -e ~/$file ]
    then
        echo "~/$file exists, not creating link..."
    else
        ln -s $BASEDIR/$file ~/$file
        echo "created link ~/$file -> $BASEDIR/$file"
    fi
done


# for filename in ".vim" ".vimrc" ".emacs"
# ln -s $BASEDIR/.vimrc ~/.vimrc
# ln -s $BASEDIR/.vim ~/.vim

# # emacs
# ln -s $BASEDIR/.emacs.d ~/.emacs.d

# # tmux
# ln -s $BASEDIR/.tmux.conf ~/.tmux.conf
