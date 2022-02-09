{ lib, pkgs }:

dir: mkconfig: { system, overlays }:

let
  removeHostname = str: builtins.head (builtins.split "@" str);

in pkgs.myme.lib.allProfiles dir (name: file: mkconfig (
  let username = removeHostname name;
  in {
    inherit system;
    homeDirectory = "/home/${username}";
    username = username;
    configuration = {
      imports = [
        (import file).home-manager.users.${username}
      ];
      config.nixpkgs.overlays = overlays;
    };
    extraSpecialArgs = {
      # Shim NixOS config on non-NixOS systems
      nixosConfig.myme = {
        de.variant = "wm";
        machine = {
          role = "desktop";
          highDPI = false;
        };
      };
    };
  }
))
