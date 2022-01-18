{ config, lib, pkgs, ... }:

with lib;

let
  lockCmd = "${./lock-wrapper.sh} ${pkgs.i3lock}/bin/i3lock -c 000000 -n";
  cfg = config.myme.wm;

in {
  imports = [
    ./alacritty
    ./fonts.nix
    ./i3
    ./polybar
    ./xmonad
  ];

  options.myme.wm = {
    enable = mkEnableOption "WM - My personal Window Manager setup";
    variant = mkOption {
      type = types.enum [ "i3" "xmonad" ];
      default = "i3";
      description = "Window Manager flavor";
    };
    plasma = mkOption {
      type = types.bool;
      default = false;
      description = "Enable KDE Plasma integration";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # Relay i3 config
      myme.wm.i3 = {
        enable = cfg.variant == "i3";
        inherit lockCmd;
      };

      # Alacritty
      myme.alacritty = {
        enable = true;
        background_opacity = 0.95;
        font_size = 6.0;
        theme = "dracula";
      };

      # Rofi
      programs.rofi = {
        enable = true;
        theme = ./rofi/dracula.rasi;
      };

      # Compositor (picom)
      services.picom.enable = true;

      # Screenshots (flameshot)
      services.flameshot.enable = true;

      # XSession
      xsession.enable = true;
      xsession.scriptPath = ".hm-xsession";
    }

    # With KDE Plasma
    (mkIf cfg.plasma {
      # Use xsession.windowManager as wm for KDE desktop
      xdg.configFile."plasma-workspace/env/wm.sh".text = ''
        export KDEWM="${config.xsession.windowManager.command}"
      '';
    })

    # Without KDE Plasma
    (mkIf (!cfg.plasma) {
      myme.polybar = {
        enable = true;
        i3gaps = cfg.variant == "i3";
        monitor = "eDP-1";
      };

      # Wallpaper (feh)
      programs.feh.enable = true;

      # Bluetooth/network
      services.blueman-applet.enable = true;

      # Network manager
      services.network-manager-applet.enable = true;

      # Screen locking
      services.screen-locker = {
        enable = true;
        lockCmd = "${lockCmd}";
      };

      # Resource monitor (conky)
      systemd.user.services.conky = {
        Unit = {
          Description = "Conky System Monitor";
          After = "graphical-session-pre.target";
          PartOf = "graphical-session.target";
        };

        Service = {
          ExecStart = "${pkgs.conky}/bin/conky -c ${./conkyrc}";
          Restart = "on-failure";
        };

        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };

      # Notifications (dunst)
      services.dunst.enable = true;
      xdg.configFile."dunst/dunstrc".source = ./dunstrc;
    })
  ]);
}
