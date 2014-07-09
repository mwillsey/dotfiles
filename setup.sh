#!/bin/sh

BASEDIR=$(dirname $0)
# sets up the symlinks

# vim
ln -s $BASEDIR/.vimrc ~/.vimrc
ln -s $BASEDIR/.vim ~/.vim

# emacs
ln -s $BASEDIR/.emacs.d ~/.emacs.d
