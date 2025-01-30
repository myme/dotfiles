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
      pkgs.alsa-utils # for volume control
    ];

    # cursor 🖱
    home.pointerCursor.hyprcursor.enable = true;

    # waybar status bar 🍫
    myme.wm.waybar.enable = true;

    # protect me precious lucky charms 🍀
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
          fade_on_empty = true;
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

    # idle handling 💤
    services.hypridle = {
      enable = true;
      settings = {
        general = {
          before_sleep_cmd = "loginctl lock-session";
          after_sleep_cmd = "hyprctl dispatch dpms on";
          ignore_dbus_inhibit = false;
          lock_cmd = "hyprlock";
        };

        listener = [
          {
            timeout = 300;
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

    # wallpapers 🖼
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

    # autoname workspaces 🤖
    systemd.user.services.hyprland-autoname-workspaces = {
      Install = { WantedBy = [ config.wayland.systemd.target ]; };

      Unit = {
        ConditionEnvironment = "WAYLAND_DISPLAY";
        Description = "hyprland-autoname-workspaces";
        After = [ config.wayland.systemd.target ];
        PartOf = [ config.wayland.systemd.target ];
      };

      Service = {
        ExecStart = "${pkgs.hyprland-autoname-workspaces}/bin/hyprland-autoname-workspaces";
        Restart = "always";
        RestartSec = "10";
      };
    };

    # my eyes! 🌄
    # TODO: Switch to hyprsunset once it supports automatic transitions
    # See: https://github.com/hyprwm/hyprsunset/issues/8
    systemd.user.services.wlsunset = {
      Install = { WantedBy = [ config.wayland.systemd.target ]; };

      Unit = {
        ConditionEnvironment = "WAYLAND_DISPLAY";
        Description = "wlsunset";
        After = [ config.wayland.systemd.target ];
        PartOf = [ config.wayland.systemd.target ];
      };

      Service = {
        ExecStart = "${pkgs.wlsunset}/bin/wlsunset -l 59.777839 -L 10.801630";
        Restart = "always";
        RestartSec = "10";
      };
    };

    # main config
    wayland.windowManager.hyprland = {
      enable = true;
      xwayland.enable = true;
      systemd.enable = true;
      extraConfig = builtins.readFile ./hyprland.conf;
    };
  };
}
