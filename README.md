# NixOS Configuration

## Installation

Ensure the machine is running and has a `root` password.

### Copy installation files to host

``` bash
$ ./bootstrap/copy.sh
```

### Start NixOS installation

``` bash
$ ./bootstrap/ssh.sh sudo ./nixos/bootstrap/build.sh
```

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
$ ./bootstrap/initNonNixOS.sh <machine>
```

### Update

``` bash
$ home-manager <build|switch> --flake .#<machine>
```

