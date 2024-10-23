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

### `hm-profiles`

List Home Manager profiles.

```bash
nix eval .#homeConfigurations --apply builtins.attrNames --json | jq -r .[]
```

### `nixos-list-generations`

```bash
sudo nix-env --profile /nix/var/nix/profiles/system --list-generations
```

### `nixos-delete-generations ${nixos-list-generations:1m}`

```bash
sudo nix-env --profile /nix/var/nix/profiles/system --delete-generations "$@"
```

## WSL

### `wsl-setup`

WSL struggles with a startup delay for SystemD where it's not immediately ready
when the WSL distro boots up. Consequently, the user services are not started at
all automatically when starting the login shell.


See: https://github.com/nix-community/NixOS-WSL/issues/375#issuecomment-2346390863

```bash
echo "Waiting for systemd..."
until systemctl &>/dev/null; do sleep 1; done

if ! systemctl --user &>/dev/null; then
    echo "Restarting user services..."
    sudo systemctl restart "user@$(id -u)"
fi

echo "Propagate WSLENV..."
systemctl --user set-environment WSLENV="$WSLENV"
```
