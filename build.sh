#!/usr/bin/env bash

# export NIX_HOST=ip_addr
# NIX_HOST=

NIX_USER=root
NIX_BLK=sda

ssh -l$NIX_USER "$NIX_HOST" " \
  echo "Creating partitions on /dev/${NIX_BLK}..." ; \
  parted /dev/${NIX_BLK} -- mklabel gpt ; \
  parted /dev/${NIX_BLK} -- mkpart primary ext4 512MiB -8GiB ; \
  parted /dev/${NIX_BLK} -- mkpart primary linux-swap -8GiB 100% ; \
  parted /dev/${NIX_BLK} -- mkpart ESP fat32 1MiB 512MiB ; \
  parted /dev/${NIX_BLK} -- set 3 esp on ; \
  echo "Creating filesystems on /dev/${NIX_BLK}..." ; \
  mkfs.ext4 -L nixos /dev/${NIX_BLK}1 ; \
  mkswap -L swap /dev/${NIX_BLK}2 ; \
  mkfs.fat -F 32 -n boot /dev/${NIX_BLK}3 ; \
  echo "Mounting partitions under /mnt and enabling swap..." ; \
  mount /dev/disk/by-label/nixos /mnt ; \
  mkdir /mnt/boot ; \
  mount /dev/disk/by-label/boot /mnt/boot ; \
  swapon /dev/disk/by-label/swap ; \
  echo "Generating nixos configuration..." ; \
  nixos-generate-config --root /mnt ; \
  echo "Updating nixos configuration..." ; \
  sed --in-place '/system\.stateVersion = .*/a \
    nix.package = pkgs.nixUnstable;\n \
    nix.extraOptions = \"experimental-features = nix-command flakes\";\n \
    services.openssh.enable = true;\n \
    services.openssh.passwordAuthentication = true;\n \
    services.openssh.permitRootLogin = \"yes\";\n \
    users.users.root.initialPassword = \"root\";\n \
  ' /mnt/etc/nixos/configuration.nix; \
  nixos-install --root /mnt --no-root-password ; \
  reboot ; \
"
