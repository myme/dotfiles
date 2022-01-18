#!/usr/bin/env bash
set -eo pipefail

source "$(dirname "$0")/setup.sh"

echo "Creating partitions on /dev/${NIX_INSTALL_BLK_DEV}..."
parted "/dev/${NIX_INSTALL_BLK_DEV}" -- mklabel gpt
parted "/dev/${NIX_INSTALL_BLK_DEV}" -- mkpart primary ext4 512MiB -8GiB
parted "/dev/${NIX_INSTALL_BLK_DEV}" -- mkpart primary linux-swap -8GiB 100%
parted "/dev/${NIX_INSTALL_BLK_DEV}" -- mkpart ESP fat32 1MiB 512MiB
parted "/dev/${NIX_INSTALL_BLK_DEV}" -- set 3 esp on

echo "Creating filesystems on /dev/${NIX_INSTALL_BLK_DEV}…"
mkfs.ext4 -L nixos "/dev/"${NIX_INSTALL_BLK_DEV}1
mkswap -L swap "/dev/${NIX_INSTALL_BLK_DEV}2"
mkfs.fat -F 32 -n boot "/dev/${NIX_INSTALL_BLK_DEV}3"

echo "Mounting partitions under /mnt and enabling swap…"
mount /dev/disk/by-label/nixos /mnt
mkdir /mnt/boot
mount /dev/disk/by-label/boot /mnt/boot
swapon /dev/disk/by-label/swap

(
    cd "$(dirname "$0")/.."

    echo "Building NixOS installation…"
    nix-shell \
        -p nixFlakes \
        -p git \
        --run "nixos-install --impure --no-root-password --flake .#${NIX_INSTALL_NAME}"

    echo "Copying installation files to user /home/${NIX_SYSTEM_USER}/nixos…"
    cp -av . "/mnt/home/${NIX_SYSTEM_USER}/nixos"
)

echo
echo "Installation complete!"
echo
read -p "Reboot? [Y/n] " -n 1 -r
echo    # (optional) move to a new line
if [[ ! $REPLY =~ ^[Nn]$ ]]
then
    reboot
fi
