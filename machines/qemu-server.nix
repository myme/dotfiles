{ config, lib, pkgs, ... }: {
  imports = [
    ../users
    ../users/myme.nix
  ];

  config = {
    myme.machine.role = "server";
    home-manager.users.myme = import ../home-manager (attrs: attrs);
  };
}
