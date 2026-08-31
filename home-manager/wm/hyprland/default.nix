{
  config,
  lib,
  pkgs,
  specialArgs,
  ...
}:

let
  cfg = config.myme.wm.hyprland;
  wallpaper = "${pkgs.myme.wallpapers}/nebula-abstract.jpg";
  hyprquit = pkgs.writeShellScriptBin "hyprquit" ''
    #!${pkgs.bash}/bin/bash
    answer="$(rofi -dmenu -p "Really quit?" <<< $'No\nYes')"
    [ "$answer" = "Yes" ] && loginctl terminate-user $USER
  '';
  inherit (specialArgs.nixosConfig.programs.hyprland) withUWSM;
  hyprctl = "${specialArgs.nixosConfig.programs.hyprland.package}/bin/hyprctl";
  # re-arm hotplugged monitors 🔌
  #
  # Hyprland can lose the first page-flip after a hotplug modeset:
  #   drm: Modesetting DP-5 with 2560x1440@59.95Hz
  #   ERR drm: Cannot commit when a page-flip is awaiting
  # The CRTC goes live and workspaces render onto it, but the panel never
  # receives a frame and reports "no signal". Neither `dispatch dpms off/on`
  # nor a bare `reload` recovers it -- both are no-ops while the mode is
  # unchanged. Only a real resolution change re-arms the flip, so bounce every
  # output through another advertised mode and let `reload` restore the
  # configured one. Monitors with a single mode have nothing to bounce
  # through and are skipped (the internal panel, typically).
  hyprmonitorbounce = pkgs.writeShellScriptBin "hyprmonitorbounce" ''
    ${hyprctl} -j monitors | ${pkgs.jq}/bin/jq -r '
      .[] | . as $m
      | [ $m.availableModes[] | select(startswith("\($m.width)x\($m.height)@") | not) ] as $alt
      | select($alt | length > 0)
      | "\($m.name) \($alt[0] | sub("Hz$";""))"
    ' | while read -r name mode; do
      ${hyprctl} keyword monitor "$name,$mode,auto,1" > /dev/null
    done
    sleep 1
    ${hyprctl} reload > /dev/null
  '';

in
{
  options.myme.wm.hyprland = {
    enable = lib.mkEnableOption "Hyprland - Tiling compositor with the looks";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      hyprmonitorbounce
      hyprquit
      pkgs.alsa-utils # for volume control
      pkgs.myme.pkgs.hyprgrab
      pkgs.nwg-displays
      pkgs.wl-clipboard
      pkgs.wlr-randr
    ];

    # cursor 🖱
    home.pointerCursor.hyprcursor.enable = true;

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
        # hyprpaper 0.8 replaced the flat `wallpaper = "monitor,path"` syntax
        # with a block; the old form is silently ignored ("no target").
        wallpaper = {
          monitor = "";
          path = wallpaper;
          # one block per monitor; empty monitor = all outputs
        };
      };
    };

    systemd.user.services = {
      # autoname workspaces 🤖
      #
      # Waybar only: renaming workspaces to "{id} {icons}" breaks quickshell,
      # which reconciles workspaces by name and grows duplicate pills under
      # dankshell. Dankshell draws app icons itself (see ../dankshell).
      hyprland-autoname-workspaces = lib.mkIf config.myme.wm.waybar.enable {
        Install = {
          WantedBy = [ config.wayland.systemd.target ];
        };

        Unit = {
          ConditionEnvironment = "WAYLAND_DISPLAY";
          Description = "hyprland-autoname-workspaces";
          After = [ config.wayland.systemd.target ];
          PartOf = [ config.wayland.systemd.target ];
        };

        Service = {
          ExecStart = "${pkgs.hyprland-autoname-workspaces}/bin/hyprland-autoname-workspaces --config ${./hyprland-autoname-workspaces.toml}";
          Restart = "always";
          RestartSec = "10";
        };
      };

      # re-arm hotplugged monitors 🔌 (see hyprmonitorbounce above)
      hyprland-monitor-bounce = {
        Install = {
          WantedBy = [ config.wayland.systemd.target ];
        };

        Unit = {
          ConditionEnvironment = "WAYLAND_DISPLAY";
          Description = "hyprland-monitor-bounce";
          After = [ config.wayland.systemd.target ];
          PartOf = [ config.wayland.systemd.target ];
        };

        Service = {
          ExecStart = toString (
            pkgs.writeShellScript "hyprland-monitor-bounce" ''
              # Glob the socket rather than trusting HYPRLAND_INSTANCE_SIGNATURE,
              # which isn't in the unit's environment on every startup ordering.
              for _ in $(seq 30); do
                socket=$(ls -t "$XDG_RUNTIME_DIR"/hypr/*/.socket2.sock 2>/dev/null | head -1)
                [ -S "$socket" ] && break
                sleep 1
              done
              [ -S "$socket" ] || exit 1

              last=0
              ${pkgs.socat}/bin/socat -U - "UNIX-CONNECT:$socket" | while read -r line; do
                case "$line" in
                  monitoradded*)
                    # A dock connect fires one event per output, but a single
                    # bounce re-modesets all of them -- ignore the rest of burst.
                    [ $((SECONDS - last)) -lt 8 ] && continue
                    sleep 2
                    ${hyprmonitorbounce}/bin/hyprmonitorbounce
                    last=$SECONDS
                    ;;
                esac
              done
            ''
          );
          Restart = "always";
          RestartSec = "10";
        };
      };

      # my eyes! 🌄
      # TODO: Switch to hyprsunset once it supports automatic transitions
      # See: https://github.com/hyprwm/hyprsunset/issues/8
      wlsunset = {
        Install = {
          WantedBy = [ config.wayland.systemd.target ];
        };

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
    };

    # main config
    wayland.windowManager.hyprland = {
      enable = true;
      configType = "hyprlang";
      xwayland.enable = true;
      systemd = {
        enable = !withUWSM;
        variables = [ "--all" ];
      };
      extraConfig = builtins.readFile ./hyprland.conf;
    };

    # Env variables for Hyprland session
    xdg.configFile."uwsm/env".source = lib.mkIf withUWSM (
      pkgs.writeTextFile {
        name = "uwsm-vars.sh";
        text = ''
          source ${config.home.sessionVariablesPackage}/etc/profile.d/hm-session-vars.sh
          unset __HM_SESS_VARS_SOURCED
        '';
      }
    );
  };
}
