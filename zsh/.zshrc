# Dumb
if [[ "$TERM" == 'dumb' ]]; then
  unsetopt zle
  PS1="$ "
  return
fi

# Starship prompt
eval "$(starship init zsh)"

# Sources
[ -d "$HOME/.zsources" ] && for f in "$HOME"/.zsources/*.zsh; do
    source "$f"
done
