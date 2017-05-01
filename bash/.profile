# .profile

# Get the aliases and functions
[ -f ~/.bashrc ] && source ~/.bashrc

export PATH="$HOME/.cargo/bin:$PATH"

# switch homes on the misl machines
if [ $HOSTNAME == "misl-a.cs.washington.edu" ]
then
    export HOME=/ssd1/home/$USER
    cd $HOME
fi
