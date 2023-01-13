#!/usr/bin/env nix-shell
#! nix-shell -i bash -p gum
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/nixos-22.11.tar.gz

set -eo pipefail

source "$(dirname "$0")/setup.sh"

echo "Choose host to copy to:"
host="$(gum choose "${NIX_INSTALL_HOST}" $(nixos_nodes))"

rsync \
    -av --delete \
    -e "ssh $SSH_OPTS -p$NIX_INSTALL_PORT" \
    . \
    "$NIX_INSTALL_USER@$host:nixos"
