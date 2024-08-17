#!/usr/bin/env bash
#
# Build a NixOS machine profile
#

set -e

if [ -z "$1" ]; then
    echo "usage: $0 <machine>" >&2
    exit 1
fi

nixos-rebuild build --flake ".#$1"
