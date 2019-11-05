# Source .profile in sh-mode if it exists
if [ -f "$HOME/.profile" ]; then
    emulate sh -c ". $HOME/.profile"
fi

