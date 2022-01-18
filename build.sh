#!/usr/bin/env bash

NIX_BLK=sda
NIX_NAME=nixos

echo "Creating partitions on /dev/${NIX_BLK}..."
parted /dev/${NIX_BLK} -- mklabel gpt
parted /dev/${NIX_BLK} -- mkpart primary ext4 512MiB -8GiB
parted /dev/${NIX_BLK} -- mkpart primary linux-swap -8GiB 100%
parted /dev/${NIX_BLK} -- mkpart ESP fat32 1MiB 512MiB
parted /dev/${NIX_BLK} -- set 3 esp on

echo "Creating filesystems on /dev/${NIX_BLK}…"
mkfs.ext4 -L nixos /dev/${NIX_BLK}1
mkswap -L swap /dev/${NIX_BLK}2
mkfs.fat -F 32 -n boot /dev/${NIX_BLK}3

echo "Mounting partitions under /mnt and enabling swap…"
mount /dev/disk/by-label/nixos /mnt
mkdir /mnt/boot
mount /dev/disk/by-label/boot /mnt/boot
swapon /dev/disk/by-label/swap

echo "Building NixOS installation…"
nix-shell \
    -p nixFlakes \
    -p git \
    --run "nixos-install --impure --no-root-password --flake .#${NIX_NAME}"

echo "Copying installation files to user…"
cp -av nixos /mnt/home/myme
