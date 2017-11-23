#! /usr/bin/env bash

# Where ssh-agent is running, for password-less access to the key
export SSH_AUTH_SOCK=$( ls /private/tmp/com.apple.launchd.*/Listeners )

files=(
    warfa:crossbot/crossbot.db
)

for f in $files; do
    rsync -az -e "ssh -T" $f $(basename $f).$(date +"%Y%m%d")
done
