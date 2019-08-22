# Pure prompt
fpath=("$HOME/.nix-profile/share/zsh/site-functions" $fpath)
export PURE_GIT_PULL=0
export RPS1=""
autoload -U promptinit; promptinit
prompt pure

# History
source ~/.zsources/history.zsh

# Aliases
alias ta="tmux attach -t"
alias tl="tmux list-sessions"
