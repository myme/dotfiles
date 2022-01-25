#!/usr/bin/env bash
#
# Build a Home Manager profile
#

set -e

# Install nix
# sh <(curl -L https://nixos.org/nix/install) --daemon

if [ -z "$1" ]; then
    echo "usage: $0 <machine>" >&2
    exit 1
fi

nix --experimental-features "nix-command flakes" build ".#homeConfigurations.$1.activationPackage"

echo
echo "Build complete! Apply to \$HOME by running:"
echo
echo "  ./result/activate"
