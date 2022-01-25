{ config, lib, pkgs, ... }@args:

with lib;

let
  lockCmd = "${./lock-wrapper.sh} ${pkgs.i3lock}/bin/i3lock -c 000000 -n";
  cfg = config.myme.wm;
  machine = args.specialArgs.nixosConfig.myme.machine;

in {
  imports = [
    ./alacritty
    ./fonts.nix
    ./i3
    ./polybar
    ./theme.nix
    ./xmonad
  ];

  options.myme.wm = {
    enable = mkEnableOption "WM - My personal Window Manager setup";
    variant = mkOption {
      type = types.enum [ "i3" "xmonad" ];
      default = "i3";
      description = "Window Manager flavor";
    };
    conky = mkOption {
      type = types.bool;
      default = true;
      description = "Enable conky resource monitor";
    };
    plasma = mkOption {
      type = types.bool;
      default =
        args.specialArgs.nixosConfig.myme.de.variant == "plasma";
      description = "Enable KDE Plasma integration";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        firefox
      ];

      # Relay i3 config
      myme.wm.i3 = {
        enable = cfg.variant == "i3";
        inherit lockCmd;
      };

      # XMonad config
      myme.wm.xmonad = if machine.highDPI then {
        fontSize = 12;
        smartBorder = false;
        spaces = 20;
      } else {
        fontSize = 10;
        smartBorder = true;
        spaces = 5;
      };

      # Alacritty
      myme.alacritty = {
        enable = true;
        background_opacity = 0.95;
        font_size = if machine.highDPI then 12 else 6;
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
      xsession = mkMerge [
        {
          enable = true;
          scriptPath = ".hm-xsession";
          initExtra = ''
            ${pkgs.feh}/bin/feh --bg-fill ${pkgs.myme.wallpapers}/alien-moon-nature.jpg
          '';
        }
        (mkIf machine.highDPI {
          profileExtra = ''
            export GDK_DPI_SCALE=1.25
            export QT_SCALE_FACTOR=1.25
          '';
        })
      ];
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
      myme.wm.polybar = mkMerge [
        {
          enable = true;
          i3gaps = cfg.variant == "i3";
        }
        (if machine.highDPI then {
          font_size = 15;
          height = 50;
        } else {
          font_size = 11;
          height = 35;
        })
      ];

      # Wallpaper (feh)
      programs.feh.enable = true;

      # Bluetooth/network
      services.blueman-applet.enable = machine.role == "laptop";

      # Network manager
      services.network-manager-applet.enable = true;

      # Screen locking
      services.screen-locker = {
        enable = true;
        lockCmd = "${lockCmd}";
      };

      # Resource monitor (conky)
      systemd.user.services.conky = (mkIf cfg.conky {
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
      });

      # Notifications (dunst)
      services.dunst = {
        enable = true;
        settings = import ./dunst.nix
          (if machine.highDPI then {
            font = "Dejavu Sans 15";
            geometry = "500x5+30+20";
          } else {
            font = "Dejavu Sans 10";
            geometry = "300x5-30+20";
          });
      };
    })
  ]);
}
