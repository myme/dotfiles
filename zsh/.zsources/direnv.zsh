# Direnv

# Output from: direnv hook zsh
# E.g. eval "$(direnv hook zsh)"
_direnv_hook_enabled=1
_direnv_hook() {
    if [ $_direnv_hook_enabled -eq 1 ]; then
        eval "$(direnv export zsh)";
    fi
}

direnv-stop() {
    pushd > /dev/null
    _direnv_hook
    _direnv_hook_enabled=0
    popd > /dev/null
}

direnv-start() {
    _direnv_hook_enabled=1
}

typeset -ag precmd_functions;

if [[ -z ${precmd_functions[(r)_direnv_hook]} ]]; then
  precmd_functions+=_direnv_hook;
fi
