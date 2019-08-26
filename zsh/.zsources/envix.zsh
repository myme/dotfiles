# Envix (pd: project cd)
pd () {
    local project;
    project=$(envix -s "$@" | tail -1)
    cd "$project"
}
