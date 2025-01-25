{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myme.wm.hyprland;
  wallpaper = "${pkgs.myme.wallpapers}/nebula-abstract.jpg";
in
{
  options.myme.wm.hyprland = {
    enable = lib.mkEnableOption "Hyprland - Tiling compositor with the looks";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.hyprsunset
      pkgs.wofi
    ];

    # waybar status bar
    myme.wm.waybar.enable = true;

    programs.hyprlock = {
      enable = true;
      settings = {
        # sample hyprlock.conf
        # for more configuration options, refer
        # https://wiki.hyprland.org/Hypr-Ecosystem/hyprlock
        general = {
          hide_cursor = true;
        };
        input-field = {
          # monitor = ;
          fade_on_empty = false;
          font_color = "rgba(255, 121, 198, 0.5)";
          inner_color = "rgba(0, 0, 0, 0.5)";
        };
        background = {
          # path = wallpaper;
          path = "screenshot";
          color = "rgb(23, 39, 41)";
          blur_passes = 2;
        };
      };
    };

    services.hypridle = {
      enable = true;
      settings = {
        general = {
          after_sleep_cmd = "hyprctl dispatch dpms on";
          ignore_dbus_inhibit = false;
          lock_cmd = "hyprlock";
        };

        listener = [
          {
            timeout = 900;
            on-timeout = "hyprlock";
          }
          {
            timeout = 1200;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
        ];
      };
    };

    services.hyprpaper = {
      enable = true;
      settings = {
        ipc = "on";
        # splash = false;
        # splash_offset = 2.0;
        preload = [ wallpaper ];
        wallpaper = [
          ",${wallpaper}"
          # "DP-1,/share/wallpapers/cat_pacman.png"
        ];
      };
    };

    wayland.windowManager.hyprland = {
      enable = true;
      xwayland.enable = true;
      systemd.enable = true;
      extraConfig = builtins.readFile ./hyprland.conf;
    };
  };
}
