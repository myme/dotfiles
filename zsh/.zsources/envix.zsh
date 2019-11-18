# Envix (p: project cd)
p () {
    local project;
    project=$(envix -s "$@" | tail -1)
    if [ -z "$project" ]; then
        return
    fi
    cd "$project"
}

# Envix (x: project exec)
x () {
    envix . "$@"
}
