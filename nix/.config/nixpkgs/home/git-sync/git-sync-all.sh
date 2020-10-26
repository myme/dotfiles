#!/usr/bin/env bash

set -euo pipefail

if [ "$#" = "0" ]; then
    echo "No repos, ignoring..." >&2
    exit 1
fi

for repo in "$@"; do
    if [ ! -d "$repo" ]; then
        echo "$repo is not a directory, ignoring..." >&2
        continue
    fi

    cd "$repo"

    if ! git-sync; then
        echo "Sync failed" "Auto sync of $repo failed." >&2
        exit 1
    fi
done
