# NixOS Configuration

## Installation

Ensure the machine is running and has a `root` password.

### Copy installation files to host

``` bash
$ ./bootstrap/copy.sh
```

### Start NixOS installation

On console:

``` bash
$ sudo NIX_INSTALL_NAME=<machine> ./bootstrap/intall.sh
```

Remotely:

``` bash
$ ./bootstrap/ssh.sh sudo NIX_INSTALL_NAME=<machine> ./nixos/bootstrap/install.sh
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
$ ./bootstrap/build-home.sh <machine>
$ ./result/activate
```

### Update

Either use the ~build-home.sh~ script above or the following once ~home-manager~
is installed:

``` bash
$ home-manager <build|switch> --flake .#<machine>
```

