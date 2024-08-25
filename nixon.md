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

### `nixos-switch`

```bash
nixos-rebuild switch --use-remote-sudo --flake .
```

### `home-manager-list-generations`

```bash
nix-env --profile $HOME/.local/state/nix/profiles/home-manager --list-generations
```

### `home-manager-delete-generations ${home-manager-list-generations:1m}`

```bash
nix-env --profile $HOME/.local/state/nix/profiles/home-manager --delete-generations "$@"
```

### `hm-activate`

Activate a Home Manager profile without doing a full `nixos-rebuild`.

```bash
hm activate
```

### `nixos-list-generations`

```bash
sudo nix-env --profile /nix/var/nix/profiles/system --list-generations
```

### `nixos-delete-generations ${nixos-list-generations:1m}`

```bash
sudo nix-env --profile /nix/var/nix/profiles/system --delete-generations "$@"
```
