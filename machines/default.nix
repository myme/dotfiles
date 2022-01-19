{ config, lib, pkgs, ... }:

with lib;

{
  options.myme.machine = {
    name = mkOption {
      type = types.str;
      default = "nixos";
      description = "Machine name";
    };
    role = mkOption {
      type = types.enum [ "desktop" "laptop" "server" ];
      default = "desktop";
      description = "Machine type";
    };
  };

  config = {
    home-manager.users.myme = {
      myme.wm = {
        bluetooth = config.myme.machine.role == "laptop";
      };
    };
  };
}
