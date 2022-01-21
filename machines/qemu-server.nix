{ config, lib, pkgs, ... }: {
  imports = [
    ../users
    ../users/myme.nix
  ];

  config = {
    # Security
    security.sudo.wheelNeedsPassword = false;

    # User config
    myme.machine.role = "server";
    home-manager.users.myme = import ../home-manager (attrs: attrs);
  };
}
