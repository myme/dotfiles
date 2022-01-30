{ config, lib, pkgs, ... }: {
  imports = [
    ../users/user.nix
  ];

  config = {
    # Security
    security.sudo.wheelNeedsPassword = false;

    # User config
    myme.machine.role = "server";
    home-manager.users.user.imports = [
      ../home-manager
    ];
  };
}
