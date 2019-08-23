# Colors: https://github.com/sorin-ionescu/prezto/blob/b8d7e2cad863959323a4c6452d58cad0af0b84e8/modules/utility/init.zsh#L76

# Call dircolors to define colors if they're missing
if [[ -z "$LS_COLORS" ]]; then
  if [[ -s "$HOME/.dir_colors" ]]; then
    eval "$(dircolors --sh "$HOME/.dir_colors")"
  else
    eval "$(dircolors --sh)"
  fi
fi

# Aliases
alias ls="${aliases[ls]:-ls} --group-directories-first --color=auto"
alias l="ls -1A"
alias ll="ls -l"
alias la="ls -la"
