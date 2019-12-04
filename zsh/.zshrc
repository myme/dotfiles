# Dumb
if [[ "$TERM" == 'dumb' ]]; then
  unsetopt zle
  PS1="$ "
  return
fi

# Add nix zsh site-functions to fpath
fpath=("$HOME/.nix-profile/share/zsh/site-functions" $fpath)

# Starship prompt
eval "$(starship init zsh)"

# Sources
[ -d "$HOME/.zsources" ] && for f in "$HOME"/.zsources/*.zsh; do
    source "$f"
done
