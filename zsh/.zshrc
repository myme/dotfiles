# Pure prompt
fpath=("$HOME/.nix-profile/share/zsh/site-functions" $fpath)
export PURE_GIT_PULL=0
export RPS1=""
autoload -U promptinit; promptinit
prompt pure

# Sources
[ -d "$HOME/.zsources" ] && for f in "$HOME"/.zsources/*.zsh; do
    source "$f"
done
