#!/usr/bin/env nix-shell
#! nix-shell -i bash -p gum -p jq
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/nixos-22.11.tar.gz

set -eo pipefail

source "$(dirname "$0")/setup.sh"

promptFormat() {
    echo "Choose disk to format"
    local disk="$(gum choose "$(lsblk -pl | grep disk | awk '{print $1}')")"

    if [ "$disk" != "none" ]; then
        gum confirm "Are you sure you want to format '$disk'?" && doFormat "$disk" || echo "Skipping format!"
    fi
}

doFormat() {
    local disk="$1"
    echo "Formatting '$disk'"

    sgdisk -d 1 "$disk"
    sgdisk -N 1 "$disk"
    partprobe "$disk"

    local part1="${disk}1"
    mkfs.ext4 -F "$part1" # wipes all data!

    mount "$part1" /mnt
}

if ! findmnt /mnt > /dev/null; then
    gum confirm "No /mnt, format disk?" && promptFormat
fi

gum confirm "Proceed with NixOS installation?" || exit 1

(
    cd "$(dirname "$0")/.."

    echo "Choose system profile to install:"
    host="$(gum choose $(nixos_hosts))"

    echo "Building NixOS installation…"
    # nixos-install --impure --no-root-password --flake .#${NIX_INSTALL_NAME}
    nixos-install --no-root-password --flake ".#${host}"

    user="$(nixos_user "$host")"
    echo "Copying installation files to user /home/${user}/nixos…"
    cp -av . "/mnt/home/${user}/nixos"
)

gum confirm "Installation complete! Reboot?" && reboot
