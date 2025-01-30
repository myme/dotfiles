{
  config,
  lib,
  pkgs,
  ...
}@args:

with lib;

let
  lockCmd = "${./lock-wrapper.sh} ${pkgs.myme.pkgs.lockscreen}/bin/lockscreen";
  cfg = config.myme.wm;
  isWayland = config.myme.wm.variant == "hyprland";
  machine = args.specialArgs.nixosConfig.myme.machine;
  onlyX11WM = machine.de.variant == "wm";
  wallpaperCmd = "${pkgs.feh}/bin/feh --bg-fill ${pkgs.myme.wallpapers}/nebula-abstract.jpg || true";

in
{
  imports = [
    ./alacritty
    ./conky
    ./gnome.nix
    ./hyprland
    ./i3
    ./polybar
    ./rofi
    ./theme.nix
    ./waybar
    ./week.nix
    ./xmonad
  ];

  options.myme.wm = {
    enable = mkEnableOption "WM - My personal Window Manager setup";
    variant = mkOption {
      type = types.enum [
        "none"
        "i3"
        "leftwm"
        "hyprland"
        "xmonad"
      ];
      default = "none";
      description = "Window Manager flavor";
    };
    dynamic_temp = mkOption {
      type = types.bool;
      default = onlyX11WM;
      description = "Dynamically change screen color temperature";
    };
    plasma = mkOption {
      type = types.bool;
      default = machine.de.variant == "plasma";
      description = "Enable KDE Plasma integration";
    };
    isWayland = mkOption {
      type = types.bool;
      default = false;
      description = "Read-only for introspection";
    };
    theme = mkOption {
      type = types.enum [
        "light"
        "dark"
      ];
      default = "dark";
      description = "System look and feel";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = [ pkgs.xclip ];

      # Install fonts
      myme.fonts.enable = true;

      # Read-only for introspection
      myme.wm.isWayland = lib.mkForce isWayland;

      # Hyprland config
      myme.wm.hyprland.enable = cfg.variant == "hyprland";

      # I3 config
      myme.wm.i3 = {
        enable = cfg.variant == "i3";
        inherit lockCmd;
      };

      # XMonad config
      myme.wm.xmonad =
        lib.mkDefault { enable = cfg.variant == "xmonad"; }
        // (
          if machine.highDPI then
            {
              fontSize = 12;
              smartBorder = false;
              spaces = 20;
            }
          else
            {
              fontSize = 10;
              smartBorder = true;
              spaces = 5;
            }
        );

      # Alacritty
      myme.alacritty = lib.mkDefault {
        enable = true;
        background_opacity = 0.95;
        font_size = if machine.highDPI then 12 else 6;
        theme = if cfg.theme == "dark" then "dracula" else "one-light";
      };

      # Emacs theme
      myme.emacs.theme = if cfg.theme == "dark" then "doom-dracula" else "doom-one-light";

      # Rofi
      myme.rofi.enable = true;

      services = mkMerge [
        {
          # Screenshots (flameshot)
          flameshot = {
            enable = true;
            package =
              if isWayland then pkgs.flameshot.override { enableWlrSupport = true; } else pkgs.flameshot;
          };
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

      # Home manager activation
      home.activation = {
        setWallpaper = lib.mkIf onlyX11WM (lib.hm.dag.entryAfter [ "writeBoundary" ] wallpaperCmd);
      };

      # XSession
      xsession = mkMerge [
        {
          enable = true;
          scriptPath = ".hm-xsession";
          initExtra = wallpaperCmd;
        }
        (mkIf (cfg.variant == "hyprland") {
          windowManager.command = "${pkgs.hyprland}/bin/hyprland";
        })
        (mkIf (cfg.variant == "leftwm") {
          windowManager.command = "${pkgs.leftwm}/bin/leftwm";
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
    (mkIf (isWayland || onlyX11WM) {
      home.packages = [
        pkgs.pavucontrol
        pkgs.pulsemixer
      ];

      # Bluetooth/network
      services.blueman-applet.enable = machine.role == "laptop";

      # Network manager
      services.network-manager-applet.enable = true;
    })

    # With Wayland
    (mkIf isWayland {
      # Use Waybar for Wayland (Hyprland)
      myme.wm.waybar = {
        enable = true;
      };
    })

    # X11 without a Desktop Environment
    (mkIf onlyX11WM {
      # Use Polybar for X11
      myme.wm.polybar = mkMerge [
        {
          enable = true;
          i3gaps = cfg.variant == "i3";
        }
        (lib.mkDefault (
          if machine.highDPI then
            {
              font_size = 15;
              height = 50;
            }
          else
            {
              font_size = 11;
              height = 35;
            }
        ))
      ];

      # Wallpaper (feh)
      programs.feh.enable = true;

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
        settings = import ./dunst.nix (
          if machine.highDPI then
            {
              font = "Dejavu Sans 15";
              geometry = "500x5+30+20";
            }
          else
            {
              font = "Dejavu Sans 10";
              geometry = "300x5-30+20";
            }
        );
      };
    })
  ]);
}
