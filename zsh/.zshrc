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

# Direnv
eval "$(direnv hook zsh)"

# Envix (pd: project cd)
pd () {
    local project;
    project=$(envix -s "$@" | tail -1)
    cd "$project"
}

# Fzf
source ~/.nix-profile/share/fzf/completion.zsh
source ~/.nix-profile/share/fzf/key-bindings.zsh
