# Envix (pd: project cd)
pd () {
    local project;
    project=$(envix -s "$@" | tail -1)
    if [ -z "$project" ]; then
        return
    fi
    cd "$project"
}
