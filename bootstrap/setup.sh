export NIX_INSTALL_HOST=${NIX_INSTALL_HOST:-localhost}
export NIX_INSTALL_PORT=${NIX_INSTALL_PORT:-2222}
export NIX_INSTALL_USER=${NIX_INSTALL_USER:-nixos}
export NIX_INSTALL_BLK_DEV=${NIX_INSTALL_BLK_DEV:-sda}
export NIX_INSTALL_NAME=${NIX_INSTALL_NAME:-nixos}

export NIX_SYSTEM_USER=${NIX_SYSTEM_USER:-myme}

export SSH_OPTS="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
