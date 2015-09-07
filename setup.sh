#!/bin/sh
# setup the dotfiles

cd $(dirname $0)
THISDIR=$(pwd)

echo
echo "Use 'y' to confirm an action, nothing to skip, and Ctrl-C to abort."
echo

find . -mindepth 1 -type d\
    -not -path "*.git*"\
    -ok mkdir -p ~/{} \;

echo

find . -mindepth 1 -type f\
    -not -path "*.git*"\
    -not -name "README.md"\
    -not -name "setup.sh"\
    -not -name "*.swp"\
    -not -name "*.swo"\
    -ok ln -s $THISDIR/{} ~/{} \;


