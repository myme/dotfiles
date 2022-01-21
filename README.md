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

