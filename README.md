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

## Updating

``` bash
$ sudo nixos-rebuild <switch|test|build> --flake .
```

## Windows Subsystem for Linux (WSL)

### Install

``` bash
$ nix --experimental-features "nix-command flakes" build .#wsl
$ ./result/activate
```

### Update

``` bash
$ home-manager <build|switch> --flake .#wsl
```

