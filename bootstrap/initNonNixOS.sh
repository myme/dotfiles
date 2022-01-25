#!/usr/bin/env bash
set -e

# Install nix
# sh <(curl -L https://nixos.org/nix/install) --daemon

if [ -z "$1" ]; then
    echo "usage: $0 <machine>" >&2
    exit 1
fi

nix --experimental-features "nix-command flakes" build ".#homeConfigurations.$1.activationPackage"

echo
echo "Build complete!"
echo
read -p "Activate? [Y/n] " -n 1 -r
echo    # (optional) move to a new line
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
    ./result/activate
fi
