{ config, lib, pkgs, ... }:

let
  cfg = config.myme.wm.waybar;

in
{
  options.myme.wm.waybar = {
    enable = lib.mkEnableOption "Enable waybar";
  };

  config = lib.mkIf cfg.enable {
    programs.waybar = {
      enable = true;
      systemd.enable = true;
      settings = {
        mainBar = {
          layer = "top";
          position = "bottom";
          height = 30;
          output = [
            "eDP-1"
            "HDMI-A-1"
          ];

          modules-left = [
            "hyprland/workspaces"
          ];

          modules-center = [
            "hyprland/window"
          ];

          modules-right = [
            "mpd"
            "idle_inhibitor"
            "custom/screen-recorder"
            "pulseaudio"
            "network"
            "power-profiles-daemon"
            "cpu"
            "memory"
            "temperature"
            "backlight"
            "keyboard-state"
            "battery"
            "clock"
            "tray"
            "custom/power"
          ];

          "hyprland/workspaces" = {
            # "format" = "{icon}";
            "format" = "{name}";
            "on-scroll-up" = "hyprctl dispatch workspace e+1";
            "on-scroll-down" = "hyprctl dispatch workspace e-1";
          };

          # Modules configuration
          # "sway/workspaces": {
          #     "disable-scroll": true,
          #     "all-outputs": true,
          #     "warp-on-scroll": false,
          #     "format": "{name}: {icon}",
          #     "format-icons": {
          #         "1": "",
          #         "2": "",
          #         "3": "",
          #         "4": "",
          #         "5": "",
          #         "urgent": "",
          #         "focused": "",
          #         "default": ""
          #     }
          # },

          "keyboard-state" = {
            "numlock" = true;
            "capslock" = true;
            "format" = "{name} {icon}";
            "format-icons" = {
              "locked" = "";
              "unlocked" = "";
            };
          };

          "sway/mode" = {
            "format" = "<span style=\"italic\">{}</span>";
          };

          "sway/scratchpad" = {
            "format" = "{icon} {count}";
            "show-empty" = false;
            "format-icons" = [
              ""
              ""
            ];
            "tooltip" = true;
            "tooltip-format" = "{app}: {title}";
          };

          "mpd" = {
            "format" =
              "{stateIcon} {consumeIcon}{randomIcon}{repeatIcon}{singleIcon}{artist} - {album} - {title} ({elapsedTime:%M:%S}/{totalTime:%M:%S}) ⸨{songPosition}|{queueLength}⸩ {volume}% ";
            "format-disconnected" = "Disconnected ";
            "format-stopped" = "{consumeIcon}{randomIcon}{repeatIcon}{singleIcon}Stopped ";
            "unknown-tag" = "N/A";
            "interval" = 5;
            "consume-icons" = {
              "on" = " ";
            };
            "random-icons" = {
              "off" = "<span color=\"#f53c3c\"></span> ";
              "on" = " ";
            };
            "repeat-icons" = {
              "on" = " ";
            };
            "single-icons" = {
              "on" = "1 ";
            };
            "state-icons" = {
              "paused" = "";
              "playing" = "";
            };
            "tooltip-format" = "MPD (connected)";
            "tooltip-format-disconnected" = "MPD (disconnected)";
          };

          "idle_inhibitor" = {
            "format" = "{icon}";
            "format-icons" = {
              "activated" = "";
              "deactivated" = "";
            };
          };

          "tray" = {
            # "icon-size": 21,
            "spacing" = 10;
          };

          "clock" = {
            # "timezone" = "America/New_York";
            "tooltip-format" = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
            "format-alt" = "{:%Y-%m-%d}";
          };

          "cpu" = {
            "format" = "{usage}% ";
            "tooltip" = false;
          };

          "memory" = {
            "format" = "{}% ";
          };

          "temperature" = {
            # "thermal-zone" = 2;
            # "hwmon-path" = "/sys/class/hwmon/hwmon2/temp1_input";
            "critical-threshold" = 80;
            # "format-critical" = "{temperatureC}°C {icon}";
            "format" = "{temperatureC}°C {icon}";
            "format-icons" = [
              ""
              ""
              ""
            ];
          };

          "backlight" = {
            # "device" = "acpi_video1";
            "format" = "{percent}% {icon}";
            "format-icons" = [
              ""
              ""
              ""
              ""
              ""
              ""
              ""
              ""
              ""
            ];
          };

          "battery" = {
            "states" = {
              # "good" = 95,
              "warning" = 30;
              "critical" = 15;
            };
            "format" = "{capacity}% {icon}";
            "format-full" = "{capacity}% {icon}";
            "format-charging" = "{capacity}% ";
            "format-plugged" = "{capacity}% ";
            "format-alt" = "{time} {icon}";
            # "format-good" = ""; // An empty format will hide the module
            # "format-full" = "";
            "format-icons" = [
              ""
              ""
              ""
              ""
              ""
            ];
          };

          "battery#bat2" = {
            "bat" = "BAT2";
          };

          "power-profiles-daemon" = {
            "format" = "{icon}";
            "tooltip-format" = "Power profile: {profile}\nDriver: {driver}";
            "tooltip" = true;
            "format-icons" = {
              "default" = "";
              "performance" = "";
              "balanced" = "";
              "power-saver" = "";
            };
          };

          "network" = {
            # "interface" = "wlp2*"; # (Optional) To force the use of this interface
            "format-wifi" = "{essid} ({signalStrength}%) ";
            "format-ethernet" = "{ipaddr}/{cidr} ";
            "tooltip-format" = "{ifname} via {gwaddr} ";
            "format-linked" = "{ifname} (No IP) ";
            "format-disconnected" = "Disconnected ⚠";
            "format-alt" = "{ifname}: {ipaddr}/{cidr}";
          };

          "pulseaudio" = {
            # "scroll-step" = 1; # %, can be a float
            "format" = "{volume}% {icon} {format_source}";
            "format-bluetooth" = "{volume}% {icon} {format_source}";
            "format-bluetooth-muted" = " {icon} {format_source}";
            "format-muted" = " {format_source}";
            "format-source" = "{volume}% ";
            "format-source-muted" = "";
            "format-icons" = {
              "headphone" = "";
              "hands-free" = "";
              "headset" = "";
              "phone" = "";
              "portable" = "";
              "car" = "";
              "default" = [
                ""
                ""
                ""
              ];
            };
            "on-click" = "amixer set Master toggle";
            "on-click-right" = "pwvucontrol";
          };

          "custom/media" = {
            "format" = "{icon} {}";
            "return-type" = "json";
            "max-length" = 40;
            "format-icons" = {
              "spotify" = "";
              "default" = "🎜";
            };
            "escape" = true;
            "exec" = "$HOME/.config/waybar/mediaplayer.py 2> /dev/null"; # Script in resources folder
            # "exec" = "$HOME/.config/waybar/mediaplayer.py --player spotify 2> /dev/null"; # Filter player based on name
          };

          "custom/power" = {
            "format" = " ⏻ ";
            "tooltip" = false;
            "menu" = "on-click";
            "menu-file" = "$HOME/.config/waybar/power_menu.xml"; # Menu file in resources folder
            "menu-actions" = {
              "shutdown" = "shutdown";
              "reboot" = "reboot";
              "suspend" = "systemctl suspend";
              "hibernate" = "systemctl hibernate";
            };
          };

          "custom/screen-recorder" = {
            "format" = " {} ";
            "exec" = "week status";
            "return-type" = "json";
            "interval" = 1;
            "on-click" = "week toggle";
          };

          # "custom/hello-from-waybar" = {
          #   format = "hello {}";
          #   interval = "once";
          #   exec = pkgs.writeShellScript "hello-from-waybar" ''
          #     echo "from within waybar"
          #   '';
          # };
        };
      };
      style = builtins.readFile ./style.css;
    };
  };
}
