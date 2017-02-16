# Overrides
export DISABLE_AUTO_TITLE="true"
export DISABLE_VENV_CD=1
export VIRTUAL_ENV_DISABLE_PROMPT="true"

# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.zsh/oh-my-zsh
export ZSH_CUSTOM=$HOME/.zsh/custom

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
# export ZSH_THEME="myme"
export ZSH_THEME="bira"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(
    brew
    cabal
    command-not-found
    fabric
    git
    git-flow
    ssh-agent
    npm
    python
    tmux
    virtualenvwrapper
    vi-mode
    zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

zle_highlight=(isearch:fg=green,bold)
