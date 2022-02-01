#!/usr/bin/env bash
set -eo pipefail

source "$(dirname "$0")/setup.sh"

ssh -tt -A "$NIX_INSTALL_HOST" -p"$NIX_INSTALL_PORT" -l"$NIX_INSTALL_USER" $SSH_OPTS "$@"
