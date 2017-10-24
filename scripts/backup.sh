#! /usr/bin/env bash

files=(
    warfa:crossbot/crossbot.db
)

for f in $files; do
    rsync -az -e ssh $f $(basename $f).$(date +"%Y%m%d")
done
