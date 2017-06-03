# .profile

# Get the aliases and functions
[ -f ~/.bashrc ] && source ~/.bashrc

# Use local for local builds

export LOCAL=$HOME/local
export PREFIX=$LOCAL

export PATH="$LOCAL/bin:$PATH"
export MANPATH="$LOCAL/share/man:$MANPATH"
export LD_LIBRARY_PATH="$LOCAL/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="$LOCAL/lib/pkgconfig:$PKG_CONFIG_PATH"
export STOW_DIR="$LOCAL/stow"

# rust binaries
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.stack/bin:$PATH"

# if go is present, put the go path on my path
if hash go 2>/dev/null; then
    export PATH="$(go env GOPATH)/bin:$PATH"
fi

# switch homes on the misl machines
if [ $HOSTNAME == "misl-a.cs.washington.edu" ]
then
    export HOME=/ssd1/home/$USER
    cd $HOME
fi

# OPAM configuration
ocaml_init=$HOME/.opam/opam-init/init.sh
[ -f $ocaml_init ] && source $ocaml_init > /dev/null 2> /dev/null
