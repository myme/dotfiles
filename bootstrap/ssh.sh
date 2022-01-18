#!/usr/bin/env bash

NIX_HOST=${NIX_HOST:-localhost}
NIX_PORT=${NIX_PORT:-2222}
NIX_USER=${NIX_USER:-root}
SSH_OPTS="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

ssh $NIX_HOST -p$NIX_PORT -l$NIX_USER $SSH_OPTS "$@"
