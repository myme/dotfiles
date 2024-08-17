# NixOS Configuration

This repository contains the [NixOS](https://nixos.org/) system and user profile
configuration files for my machines. For an overview and more thorough
description, read [NixOS: Confederation
(myme.no)](https://myme.no/posts/2022-06-14-nixos-confederation.html).

I rarely install new machines and thus haven't really invested the time to
automate this process in a good way. The steps under
[Installation](#installation-experimentalbroken) are most likely borken or
incomplete. Your best bet is to [download and
install](https://nixos.org/download.html#nixos-iso) `NixOS` following the
regular documentation, then simply [build and update](#updating) using
`nixos-rebuild switch --flake .`.

## Installation

### Install SSH keys on machine

Use a local keyboard and mouse or remote console to start up a shell on the
machine. Then add public keys as desired to `.ssh/authorized_keys` for remote
access.

``` bash
$ mkdir .ssh
$ curl -o .ssh/authorized_keys https://github.com/myme.keys
```

### Jump into dev shell

``` bash
# shorthand for `nix develop` (with --extra-experimental-features)
./dev
```

### Copy installation files to host

The following command will prompt for one of the `deploy.nodes` hosts to copy
the installation files to.

``` bash
$ nixbs-copy
```

Alternatively export `NIX_INSTALL_HOST` in the environment to override:

``` bash
$ NIX_INSTALL_HOST=10.20.30.40 nixbs-copy
```

### SSH to remote host

``` bash
$ nixbs-ssh
```

### Drop into dev shell

``` bash
$ cd nixos
$ ./dev
```

### Format disks with `disko`

``` bash
$ sudo disko ./machine/<hostname>/disk.nix
```

### Start NixOS installation

Begin the installation by invoking the install script, either on console or over
`SSH`:

``` bash
$ sudo nixbs-install
```

The command will prompt for a system profile to install.

### Post-install

Installing `emacs` dependencies:

``` bash
$ ~/.emacs.d/bin/doom sync
```

After launching `emacs`, install `all-the-icons`:

``` emacs-lisp
(all-the-icons-install-fonts t)
```

## Updating

``` bash
$ sudo nixos-rebuild <switch|test|build> --flake .
```

## Non-NixOS Linux / Windows Subsystem for Linux (WSL)

### Install

``` bash
$ nixbs-build-home <machine>
$ ./result/activate
```

### Update

Either use the `build-home.sh` script above or the following once [Home
Manager](https://github.com/nix-community/home-manager) is installed:

``` bash
$ home-manager <build|switch> --flake .#<machine>
```

