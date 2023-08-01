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

### Copy installation files to host

The following command will prompt for one of the `deploy.nodes` hosts to copy
the installation files to.

``` bash
$ ./bootstrap/copy.sh
```

### Start NixOS installation

Begin the installation by invoking the install script, either on console or over
`SSH`:

``` bash
$ sudo ./bootstrap/install.sh
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
$ ./bootstrap/build-home.sh <machine>
$ ./result/activate
```

### Update

Either use the `build-home.sh` script above or the following once [Home
Manager](https://github.com/nix-community/home-manager) is installed:

``` bash
$ home-manager <build|switch> --flake .#<machine>
```

