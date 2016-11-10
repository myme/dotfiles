venv=$(basename "$VIRTUAL_ENV")
if [ -n "$venv" ]; then
    workon -n "$venv"
fi
