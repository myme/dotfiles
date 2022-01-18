# NixOS Configuration

## Installation

Ensure the machine is running and has a `root` password.

### Copy installation files to host

``` bash
$ ./bootstrap/copy.sh
```

### Start NixOS installation

``` bash
$ ./bootstrap/ssh.sh "cd nixos && ./bootstrap/build.sh"
```

