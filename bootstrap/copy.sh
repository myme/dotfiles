#!/usr/bin/env bash

# export NIX_HOST=ip_addr
NIX_HOST=${NIX_HOST:-localhost}
NIX_PORT=${NIX_PORT:-2222}
NIX_USER=${NIX_USER:-root}

SSH_OPTS="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

rsync -av --delete -e "ssh $SSH_OPTS -p$NIX_PORT" . "$NIX_USER@$NIX_HOST:nixos"
