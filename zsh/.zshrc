#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

eval `keychain --eval -q ~/.ssh/*_rsa`

export EDITOR="ec"

if [[ -d "$HOME/.zsources" ]]; then
    for src in "$HOME"/.zsources/*; do
        source "$src"
    done
fi
