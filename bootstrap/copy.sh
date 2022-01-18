#!/usr/bin/env bash
set -eo pipefail

source "$(dirname "$0")/setup.sh"

rsync \
    -av --delete \
    -e "ssh $SSH_OPTS -p$NIX_INSTALL_PORT" \
    . \
    "$NIX_INSTALL_USER@$NIX_INSTALL_HOST:nixos"
