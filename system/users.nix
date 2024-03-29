{ config, lib, ... }:

let
  cfg = config.myme.machine;
  genericLinux = cfg.flavor == "generic";

in {
  options.myme.machine.user = {
    name = lib.mkOption {
      type = lib.types.str;
      default = "nixos";
      description = "Machine main user name.";
    };
    config = lib.mkOption {
      type = lib.types.anything;
      default = {};
      description = "NixOS user config.";
    };
    profile = lib.mkOption {
      type = lib.types.anything;
      default = {};
      description = "User Home Manager profile.";
    };
  };

  config = {
    # Home manager
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = !genericLinux;

    users.users.${cfg.user.name} = cfg.user.config;
    home-manager.users.${cfg.user.name} = lib.mkMerge [
      cfg.user.profile
      ({ specialArgs, ... }: {
        config = {
          # Pass stateVersion from NixOS config
          home.stateVersion = specialArgs.nixosConfig.system.stateVersion;

          submoduleSupport.enable = lib.mkForce (!genericLinux);
          targets.genericLinux.enable = genericLinux;
        };
      })
    ];
  };
}
