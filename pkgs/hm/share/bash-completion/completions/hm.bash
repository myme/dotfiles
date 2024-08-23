_hm_completion() {
    COMPREPLY=()
    local cur="${COMP_WORDS[COMP_CWORD]}"
    local prev="${COMP_WORDS[COMP_CWORD-1]}"

    # Get all commands from the `hm` script
    local script_path=$(which hm 2>/dev/null)
    if [[ -z "$script_path" ]]; then
        script_path="./hm"
    fi
    local commands=$(grep -E '^ +[a-z_]+\)' "$script_path" | \
      sed 's/)//' | sed 's;#.*;;g' | sed 's/ //g' | sort)

    # If we're on the first word, suggest commands
    if [[ ${COMP_CWORD} -eq 1 ]]; then
        COMPREPLY=($(compgen -W "$commands" -- "$cur"))
        return 0
    fi

    # Add more specific completions here if needed
    # For example, if certain commands take specific arguments

    return 0
}

complete -F _hm_completion hm
