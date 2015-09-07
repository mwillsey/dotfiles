#!/bin/sh
# setup the dotfiles

cd $(dirname $0)
THISDIR=$(pwd)

echo
echo "Creating directory structure"
echo

find . -type d -mindepth 1\
    -not -path "*.git*"\
    -exec echo "mkdir -p ~/{}" \;\
    -exec mkdir -p ~/{} \;

echo
echo "Any key to confirm symlink creation; Ctrl-C to abort."
echo

find . -type f -mindepth 1\
    -not -path "*.git*"\
    -not -name "README.md"\
    -not -name "setup.sh"\
    -not -name "*.swp"\
    -not -name "*.swo"\
    -exec read -p "Confirm 'ln -s $THISDIR/{} ~/{}':" \;\
    -exec ln -s $THISDIR/{} ~/{} \;


