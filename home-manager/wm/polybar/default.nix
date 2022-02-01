{ config, lib, pkgs, ... }:

with lib;

let
  wm = config.myme.wm;
  cfg = wm.polybar;
  polybar = config.services.polybar.package;
  themes = {
    dracula = import ./dracula-colors.nix;
    nord = import ./nord-colors.nix;
  };
  theme = themes.dracula;

in {
  options = {
    myme.wm.polybar = {
      enable = mkEnableOption "Polybar - Window manager status bar.";
      i3gaps = mkEnableOption "Enable i3gaps support to polybar.";
      monitor = mkOption {
        type = types.str;
        description = "Monitor on which to place the bar.";
      };
      font_size = mkOption {
        type = types.int;
        default = 12;
        description = "Polybar font size";
      };
      height = mkOption {
        type = types.int;
        default = 35;
        description = "Polybar height";
      };
    };
  };

  config = let
    font_size = builtins.toString cfg.font_size;
    font_size_material =
      builtins.toString (cfg.font_size + (cfg.font_size / 7));
  in {
    systemd.user.services.polybar = {
      Service.Environment =
        mkForce "PATH=${polybar}/bin:/run/wrappers/bin:${pkgs.xdotool}/bin";
    };
    services.polybar = {
      enable = cfg.enable;
      package = pkgs.polybar.override { i3GapsSupport = cfg.i3gaps; };
      config = {
        defaults = {
          width = "100%";
          height = cfg.height;
          fixed-center = true;
          bottom = true;

          module-margin-left = 1;
          module-margin-right = 2;

          background = theme.colors.background;
          foreground = theme.colors.foreground;

          line-size = 3;
          line-color = theme.colors.urgent;

          font-0 = "Dejavu Sans Mono for Powerline:pixelsize=${font_size};1";
          font-1 = "FontAwesome:size=${font_size};1";
          font-2 = "Material Icons:size=${font_size_material};2";

          cursor-click = "pointer";
          cursor-scroll = "ns-resize";
        };

        "bar/main" = {
          "inherit" = "defaults";
          monitor = "\${env:MONITOR:${cfg.monitor}}";

          fixed-center = false;

          modules-left =
            if wm.variant == "xmonad" then "xmonad" else "xworkspaces";
          modules-center = "xwindow";
          modules-right = [
            "alsa"
            "wlan"
            "eth"
            "battery"
            "backlight"
            "cpu"
            "memory"
            "filesystem"
            "date"
          ];

          tray-position = "right";
        };

        "bar/aux" = {
          "inherit" = "defaults";
          monitor = "\${env:MONITOR:DP-2}";
          modules-left =
            if wm.variant == "xmonad" then "xmonad" else "xworkspaces";
          modules-center = [ "xwindow" ];
          modules-right = [ "date" ];
        };

        "module/xmonad" = {
          type = "custom/script";
          exec =
            "${pkgs.coreutils}/bin/tail -F \${XDG_RUNTIME_DIR:-/tmp}/xmonad.log";
          tail = true;
        };

        "module/xworkspaces" = {
          type = "internal/xworkspaces";

          label-active = "%name%";
          label-active-background = theme.colors.background-alt;
          label-active-underline = theme.colors.primary;
          label-active-padding = 2;

          label-empty = "%name%";
          label-empty-padding = 2;

          label-urgent = "%name%";
          label-urgent-background = theme.colors.alert;
          label-urgent-padding = 2;
        };

        "module/xwindow" = {
          type = "internal/xwindow";
          label = "%title:0:30:...%";
        };

        "module/alsa" = {
          type = "internal/alsa";

          format-volume-prefix = " ";
          format-volume = "<label-volume> <bar-volume>";
          label-volume-foreground = theme.colors.foreground;

          format-muted-prefix = " ";
          format-muted-foreground = theme.colors.foreground-alt;
          label-muted = "sound muted";

          bar-volume-width = 10;
          bar-volume-foreground-0 = theme.colors.success;
          bar-volume-foreground-1 = theme.colors.success;
          bar-volume-foreground-2 = theme.colors.success;
          bar-volume-foreground-3 = theme.colors.success;
          bar-volume-foreground-4 = theme.colors.success;
          bar-volume-foreground-5 = theme.colors.notify;
          bar-volume-foreground-6 = theme.colors.urgent;
          bar-volume-gradient = false;
          bar-volume-indicator = "|";
          bar-volume-indicator-font = 2;
          bar-volume-fill = "─";
          bar-volume-fill-font = 2;
          bar-volume-empty = "─";
          bar-volume-empty-font = 2;
          bar-volume-empty-foreground = theme.colors.foreground-alt;
        };

        "module/cpu" = {
          type = "internal/cpu";
          interval = 2;
          format-prefix = " ";
          format-prefix-foreground = theme.colors.foreground-alt;
          format-underline = theme.colors.urgent;
          label = "%percentage:2%%";
        };

        "module/memory" = {
          type = "internal/memory";
          interval = 2;
          format-prefix = " ";
          format-prefix-foreground = theme.colors.foreground-alt;
          format-underline = theme.colors.warning;
          label = "%percentage_used%%";
        };

        "module/filesystem" = {
          type = "internal/fs";
          interval = 10;
          mount-0 = "/";
          label-mounted = " %mountpoint% %percentage_used%%";
          label-mounted-underline = theme.colors.notify;
          label-unmounted = " %mountpoint% not mounted";
          label-unmounted-foreground = theme.colors.foreground-alt;
          label-unmounted-underline = theme.colors.alert;
        };

        "module/eth" = {
          type = "internal/network";
          interface = "ens33";
          interval = 3;

          format-connected-underline = theme.colors.function;
          format-connected-prefix = " ";
          format-connected-prefix-foreground = theme.colors.foreground-alt;
          label-connected = "%local_ip%";

          format-disconnected = "";
        };

        "module/wlan" = {
          type = "internal/network";
          interface = "wlp1s0";
          interval = 3;

          format-connected = " <label-connected>";
          format-connected-underline = theme.colors.function;
          label-connected = "%essid%";

          format-disconnected = "";

          ramp-signal-0 = "";
          ramp-signal-1 = "";
          ramp-signal-2 = "";
          ramp-signal-3 = "";
          ramp-signal-4 = "";
          ramp-signal-foreground = theme.colors.foreground-alt;
        };

        "module/battery" = {
          type = "internal/battery";
          battery = "BAT0";
          adapter = "ADP1";
          full-at = 98;

          format-charging = "<animation-charging> <label-charging>";
          format-charging-underline = theme.colors.notify;

          format-discharging = "<ramp-capacity> <label-discharging>";
          format-discharging-underline = "\${self.format-charging-underline}";

          format-full-prefix = " ";
          format-full-prefix-foreground = theme.colors.foreground-alt;
          format-full-underline = "\${self.format-charging-underline}";

          ramp-capacity-0 = "";
          ramp-capacity-1 = "";
          ramp-capacity-2 = "";
          ramp-capacity-3 = "";
          ramp-capacity-4 = "";
          ramp-capacity-foreground = theme.colors.foreground-alt;

          animation-charging-0 = "";
          animation-charging-1 = "";
          animation-charging-2 = "";
          animation-charging-3 = "";
          animation-charging-4 = "";
          animation-charging-foreground = theme.colors.foreground-alt;
          animation-charging-framerate = 750;

        };

        "module/backlight" = {
          type = "internal/backlight";
          card = "intel_backlight";

          # Available tags:
          #   <label> (default)
          #   <ramp>
          #   <bar>
          format = "<ramp> <label>";
          format-underline = theme.colors.success;

          # Available tokens:
          #   %percentage% (default)
          label = "%percentage%%";

          # Only applies if <ramp> is used
          ramp-0 = "";
          ramp-1 = "";
          ramp-2 = "";

          # Only applies if <bar> is used
          bar-width = 10;
          bar-indicator = "|";
          bar-fill = "─";
          bar-empty = "─";
        };

        "module/date" = {
          type = "internal/date";
          interval = 5;

          date = "";
          date-alt = " %Y-%m-%d";

          time = "%H:%M";
          time-alt = "%H:%M:%S";

          format-prefix = "";
          format-prefix-foreground = theme.colors.foreground-alt;
          format-underline = theme.colors.info;

          label = "%date% %time% ";
        };
      };
      script = ''
        MONITOR=${cfg.monitor} ${polybar}/bin/polybar main &
      '';
    };
  };
}
