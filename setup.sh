#!/usr/bin/env bash

# get this directory
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

[ -z "$LOCAL" ] && echo "set LOCAL to something" && exit 1

mk_link () {
   source=$1
   target=$2

   # check for existing file or broken symlink
   if [ -e "$target" ]; then
       real_source=`realpath "$source"`
       real_target=`realpath "$target"`
       if [ "$real_source" = "$real_target" ]; then
           echo "OK.     '$source' already linked."
           true
       else
           >&2 echo "SKIP!   File '$target' exists."
       fi
   elif [ -h "$target" ]; then
       >&2 echo "SKIP!   Broken symlink '$target' exists."
   else
       ln -s "$source" "$target"
       echo "LINKED! '$source' -> '$target'"
   fi
}

cd $DIR/dotfiles
for file in *; do
   mk_link "$(pwd)/$file" "$HOME/.$file"
done

cd $DIR/config
for file in *; do
   mk_link "$(pwd)/$file" "$HOME/.config/$file"
done

cd $DIR/scripts
for file in *; do
   mk_link "$(pwd)/$file" "$LOCAL/bin/$file"
done

# manually install other
cd $DIR/other/vscode
for file in *; do
   mk_link "$(pwd)/$file" "$HOME/Library/Application Support/Code/User/$file"
done
