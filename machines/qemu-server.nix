{ config, lib, pkgs, ... }: {
  imports = [
    ../users
    ../users/myme.nix
  ];

  config = {
    # Network
    networking.interfaces.ens3.useDHCP = true;

    # User config
    myme.machine.role = "server";
    home-manager.users.myme = import ../home-manager (attrs: attrs);
  };
}
