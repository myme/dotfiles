{ config, lib, pkgs, ... }:

with lib;

let
  machine = config.myme.machine;
  cfg = machine.de;
  xserver = (config.myme.machine.role != "server" && config.myme.machine.flavor
    != "wsl");

in {
  options = {
    myme.machine.highDPI = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Optimize for high DPI outputs (4k)";
    };
    myme.machine.de.variant = mkOption {
      type = types.enum [ "none" "gnome" "plasma" "wm" "xfce" ];
      default = if xserver then "wm" else "none";
      description = "Desktop Environment flavor";
    };
  };

  config = mkMerge [
    (mkIf (cfg.variant != "none") {
      services.xserver = {
        enable = true;
        xkb = { layout = "us"; variant = "alt-intl-unicode"; };
      };

      # Enable touchpad support
      services.libinput = {
        enable = true;
        touchpad.naturalScrolling = true;
      };

      # LightDM Background image
      services.xserver.displayManager.lightdm.background =
        "${pkgs.myme.wallpapers}/alien-moon-nature.jpg";

      # Disable xterm session
      services.xserver.desktopManager.xterm.enable = false;
    })
    # WM
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
    (mkIf (machine.role == "laptop" && cfg.variant == "wm") {
      # Backlight
      services.illum.enable = true;
    })
    # Gnome
    (mkIf (cfg.variant == "gnome") {
      services.xserver.displayManager.gdm.enable = true;
      services.xserver.desktopManager.gnome = {
        enable = true;
        flashback.customSessions = [{
          wmName = "hmxsession";
          wmLabel = "HomeManager XSession";
          wmCommand = let
            wmCommand = pkgs.writeShellScript "hm-xsession" ''
              $HOME/.hm-xsession
            '';
          in "${wmCommand}";
          enableGnomePanel = false;
        }];
      };
    })
    # KDE Plasma
    (mkIf (cfg.variant == "plasma") {
      services.xserver.displayManager.sddm.enable = true;
      services.xserver.desktopManager.plasma5.enable = true;
    })
    # XFCE
    (mkIf (cfg.variant == "xfce") {
      # Home manager xsession
      services.xserver.windowManager.session = [{
        name = "home-manager";
        start = ''
          $HOME/.hm-xsession
        '';
      }];
      services.xserver.desktopManager = {
        xfce = {
          enable = true;
          noDesktop = true;
          enableXfwm = false;
        };
      };
    })
  ];
}
