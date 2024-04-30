export NIX_INSTALL_HOST=${NIX_INSTALL_HOST:-localhost}
export NIX_INSTALL_PORT=${NIX_INSTALL_PORT:-22}
export NIX_INSTALL_USER=${NIX_INSTALL_USER:-nixos}
export NIX_INSTALL_BLK_DEV=${NIX_INSTALL_BLK_DEV:-sda}
export NIX_INSTALL_NAME=${NIX_INSTALL_NAME:-nixos}

export NIX_SYSTEM_USER=${NIX_SYSTEM_USER:-myme}

export SSH_OPTS="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

nixos_hosts() {
    nix eval \
        --json \
        --extra-experimental-features 'nix-command flakes' \
        .#nixosConfigurations \
        --apply builtins.attrNames | jq -r '.[]'
}

nixos_nodes() {
    nix eval \
        --json \
        --extra-experimental-features 'nix-command flakes' \
        .#deploy.nodes \
        --apply builtins.attrNames | jq -r '.[]'
}

nixos_user() {
    local host="$1"
    nix eval \
        --raw \
        --extra-experimental-features 'nix-command flakes' \
        ".#nixosConfigurations.${host}.config.myme.machine.user.name"
}

nixos_ssh() {
    local host="${NIX_INSTALL_HOST:-$1}"
    ssh $SSH_OPTS $NIX_INSTALL_USER@$host -p $NIX_INSTALL_PORT
}
