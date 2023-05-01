# Nixon commands

## Emacs

### `emacs-restart`

```bash
systemctl --user stop emacs.{service,socket}
systemctl --user start emacs.socket
```

### `doom-sync`

```bash
~/.emacs.d/bin/doom sync
```

## NixOS

### `_nixos-configurations`

```bash
nix flake show --json | jq -r '.nixosConfigurations | keys[]'
```

### `nixos-test`

```bash
nixos-rebuild test --use-remote-sudo --flake .
```

### `nixos-rebuild ${_nixos-rebuild-target} ${_nixos-configurations}`

```bash
nixos-rebuild "$1" --use-remote-sudo --flake ".#$2"
```

### `_nixos-rebuild-target`

```plain
build
test
switch
```

### `nixos-list-generations`

```bash
sudo nix-env --list-generations --profile /nix/var/nix/profiles/system
```

