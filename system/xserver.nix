{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.myme.de;
  xserver = (
    config.myme.machine.role != "server" &&
    config.myme.machine.flavor != "wsl");

in {
  options = {
    myme.machine.highDPI = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Optimize for high DPI outputs (4k)";
    };
    myme.de.variant = mkOption {
      type = types.enum [ "none" "plasma" "wm" ];
      default = if xserver then "wm" else "none";
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

      # LightDM Background image
      services.xserver.displayManager.lightdm.background = "${pkgs.myme.wallpapers}/alien-moon-nature.jpg";
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

      # Session - gnome-keyring - https://github.com/jluttine/NiDE/blob/master/src/keyring.nix
      programs.dconf.enable = true;
      services.gnome.gnome-keyring.enable = true;
      security.pam.services.xdm.enableGnomeKeyring = true;
    })
    (mkIf (cfg.variant == "plasma") {
      services.xserver.displayManager.sddm.enable = true;
      services.xserver.desktopManager.plasma5.enable = true;
    })
  ];
}
