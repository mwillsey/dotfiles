# Use $EDITOR to edit the current command
function edit_command
    set -q EDITOR; or return 1
    set -l tmpfile (mktemp /tmp/command.XXXX); or return 1
    commandline > $tmpfile
    eval $EDITOR $tmpfile
    commandline -r -- (cat $tmpfile)
    rm $tmpfile
end

function fish_user_key_bindings
    # bind edit_command to ESC to work like vim
    bind \e edit_command
end

