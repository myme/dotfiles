#!/usr/bin/env bash

set -eo pipefail

if xclip -t TARGETS -o | grep -q text/html; then
    xclip -o -selection clipboard -t text/html \
        | pandoc -f html -t json \
        | pandoc -f json -t org --wrap none \
        | sed 's/ / /g'
else
    xclip -o -selection clipboard
fi
