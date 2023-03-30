{ config, lib, pkgs, ... }@args:

with lib;

let
  lockCmd = "${./lock-wrapper.sh} ${pkgs.myme.pkgs.lockscreen}/bin/lockscreen";
  cfg = config.myme.wm;
  machine = args.specialArgs.nixosConfig.myme.machine;
  noDM = machine.de.variant != "wm";

in {
  imports = [
    ./alacritty
    ./conky
    ./i3
    ./polybar
    ./rofi
    ./theme.nix
    ./xmonad
  ];

  options.myme.wm = {
    enable = mkEnableOption "WM - My personal Window Manager setup";
    variant = mkOption {
      type = types.enum [ "i3" "leftwm" "xmonad" ];
      default = "i3";
      description = "Window Manager flavor";
    };
    dynamic_temp = mkOption {
      type = types.bool;
      default = !noDM;
      description = "Dynamically change screen color temperature";
    };
    plasma = mkOption {
      type = types.bool;
      default = machine.de.variant == "plasma";
      description = "Enable KDE Plasma integration";
    };
    theme = mkOption {
      type = types.enum [ "light" "dark" ];
      default = "dark";
      description = "System look and feel";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = [ pkgs.xclip ];

      # Install fonts
      myme.fonts.enable = true;

      # Relay i3 config
      myme.wm.i3 = {
        enable = cfg.variant == "i3";
        inherit lockCmd;
      };

      # XMonad config
      myme.wm.xmonad = lib.mkDefault { enable = cfg.variant == "xmonad"; } // (
        if machine.highDPI then {
          fontSize = 12;
          smartBorder = false;
          spaces = 20;
        } else {
          fontSize = 10;
          smartBorder = true;
          spaces = 5;
        });

      # Alacritty
      myme.alacritty = lib.mkDefault {
        enable = true;
        background_opacity = 0.95;
        font_size = if machine.highDPI then 12 else 6;
        theme = if cfg.theme == "dark" then "dracula" else "one-light";
      };

      # Emacs theme
      myme.emacs.theme =
        if cfg.theme == "dark" then "doom-dracula" else "doom-one-light";

      # Rofi
      myme.rofi.enable = true;

      services = mkMerge [
        {
          # Screenshots (flameshot)
          flameshot.enable = true;
        }

        # Dynamic temperature (redshift)
        (mkIf cfg.dynamic_temp {
          redshift = {
            enable = true;
            latitude = "59.777839";
            longitude = "10.801630";
            tray = true;
          };
        })
      ];

      # XSession
      xsession = mkMerge [
        {
          enable = true;
          scriptPath = ".hm-xsession";
          initExtra = ''
            ${pkgs.feh}/bin/feh --bg-fill ${pkgs.myme.wallpapers}/alien-moon-nature.jpg
          '';
        }
        (mkIf (cfg.variant == "leftwm") {
          windowManager.command = "${pkgs.leftwm}/bin/leftwm";
          # windowManager.command = "${pkgs.i3}/bin/i3";
        })
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

    # Without a Desktop Environment
    (mkIf (!noDM) {
      home.packages = with pkgs; [ pkgs.pavucontrol ];

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

      # Compositor (picom)
      services.picom.enable = true;

      # Screen locking
      services.screen-locker = {
        enable = true;
        lockCmd = "${lockCmd}";
      };

      # Notifications (dunst)
      services.dunst = {
        enable = true;
        settings = import ./dunst.nix (if machine.highDPI then {
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
