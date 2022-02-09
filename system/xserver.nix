{ config, lib, pkgs, ... }:

with lib;

let cfg = config.myme.de;

in {
  options = {
    myme.de.variant = mkOption {
      type = types.enum [ "none" "plasma" "wm" ];
      default = if config.myme.machine.role == "server" then "none" else "wm";
      description = "Desktop Environment flavor";
    };
  };

  config = mkMerge [
    (mkIf (cfg.variant != "none") {
      services.xserver.enable = true;
      services.xserver.layout = "us";
      services.xserver.xkbVariant = "alt-intl-unicode";

      # Enable touchpad support
      services.xserver.libinput.enable = true;
      services.xserver.libinput.touchpad.naturalScrolling = true;
    })
    (mkIf (cfg.variant == "wm") {
      # Home manager xsession
      services.xserver.desktopManager.session = [{
        name = "home-manager";
        start = ''
          ${pkgs.runtimeShell} $HOME/.hm-xsession &
          waitPID=$!
        '';
      }];
    })
    (mkIf (cfg.variant == "plasma") {
      services.xserver.displayManager.sddm.enable = true;
      services.xserver.desktopManager.plasma5.enable = true;
    })
  ];
}
