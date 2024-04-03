{ config, lib, pkgs, ... }:

let
  cfg = config.myme.wm.i3;
  modifier = config.xsession.windowManager.i3.config.modifier;
  exitCmd = "i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";
  lockCmd = cfg.lockCmd;
  hibernateCmd = "i3-nagbar -t warning -m 'Do you want to hibernate?' -b 'Yes' 'systemctl hibernate'";
  suspendCmd = "${lockCmd} && systemctl suspend";
  rebootCmd = "i3-nagbar -t warning -m 'Do you want to reboot?' -b 'Yes' 'systemctl reboot'";
  shutdownCmd = "i3-nagbar -t warning -m 'Do you want to shutdown?' -b 'Yes' 'systemctl poweroff'";
  lockMode = "(l) lock | (e) logout | (s) suspend | (h) hibernate | (r) reboot | (p) shutdown";
  # backlight = "${pkgs.light}/bin/light -s sysfs/backlight/intel_backlight";
  amixer = "${pkgs.alsa-utils}/bin/amixer";
  nixonCfg = config.programs.nixon;

in {
  imports = [
    ./i3ws.nix
  ];

  options.myme.wm.i3 = {
    enable = lib.mkEnableOption "i3 - The tiling window manager.";

    plasma = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable KDE Plasma integration";
    };

    lockCmd = lib.mkOption {
      type = lib.types.str;
      description = "Screen locking command";
    };
  };

  config = lib.mkIf cfg.enable {
    # i3ws
    myme.i3ws = {
      enable = cfg.enable;
      icons = true;
      separator = " ";
    };

    # i3 config
    xsession.windowManager.i3 = {
      enable = cfg.enable;
      package = pkgs.i3-gaps;
      config = {
        modifier = "Mod4";
        bars = [];
        colors = {
          background = "#ffffff";
          focused         = { border = "#4c7899"; background = "#285577"; text = "#ffffff"; indicator = "#2e9ef4"; childBorder = "#285577"; };
          focusedInactive = { border = "#333333"; background = "#5f676a"; text = "#ffffff"; indicator = "#484e50"; childBorder = "#5f676a"; };
          unfocused       = { border = "#333333"; background = "#222222"; text = "#888888"; indicator = "#292d2e"; childBorder = "#222222"; };
          urgent          = { border = "#2f343a"; background = "#900000"; text = "#ffffff"; indicator = "#900000"; childBorder = "#900000"; };
          placeholder     = { border = "#000000"; background = "#0c0c0c"; text = "#ffffff"; indicator = "#000000"; childBorder = "#0c0c0c"; };
        };
        fonts = {
          names = ["Dejavu Sans Mono" "FontAwesome 11"];
        };
        gaps = {
          inner = 10;
          smartBorders = "on";
          smartGaps = true;
        };
        floating.criteria = [
          { class = "^davmail"; }
          { class = "^openconnect-sso"; }
        ];
        keybindings = lib.mkOptionDefault (
          {
            # Terminal
            "${modifier}+Return" = "exec x-terminal-emulator -e tmux";
            "${modifier}+Shift+Return" = "exec x-terminal-emulator";

            # Command
            "${modifier}+Shift+semicolon" = "exec --no-startup-id i3-input -P 'Command: '";

            # Workspaces
            "${modifier}+p" = "workspace prev";
            "${modifier}+n" = "workspace next";

            # Switch to workspace number
            "${modifier}+1" = "workspace number 1";
            "${modifier}+2" = "workspace number 2";
            "${modifier}+3" = "workspace number 3";
            "${modifier}+4" = "workspace number 4";
            "${modifier}+5" = "workspace number 5";
            "${modifier}+6" = "workspace number 6";
            "${modifier}+7" = "workspace number 7";
            "${modifier}+8" = "workspace number 8";
            "${modifier}+9" = "workspace number 9";
            "${modifier}+0" = "workspace number 10";

            # Move containers to workspaces
            "${modifier}+Shift+1" = "move container to workspace number 1";
            "${modifier}+Shift+2" = "move container to workspace number 2";
            "${modifier}+Shift+3" = "move container to workspace number 3";
            "${modifier}+Shift+4" = "move container to workspace number 4";
            "${modifier}+Shift+5" = "move container to workspace number 5";
            "${modifier}+Shift+6" = "move container to workspace number 6";
            "${modifier}+Shift+7" = "move container to workspace number 7";
            "${modifier}+Shift+8" = "move container to workspace number 8";
            "${modifier}+Shift+9" = "move container to workspace number 9";
            "${modifier}+Shift+0" = "move container to workspace number 10";

            # Move workspace to outputs
            "${modifier}+Control+Shift+h" = "move workspace to output left";
            "${modifier}+Control+Shift+l" = "move workspace to output right";

            # Vim focus + move
            "${modifier}+h" = "focus left";
            "${modifier}+l" = "focus right";
            "${modifier}+j" = "focus down";
            "${modifier}+k" = "focus up";

            "${modifier}+Shift+h" = "move left";
            "${modifier}+Shift+l" = "move right";
            "${modifier}+Shift+j" = "move down";
            "${modifier}+Shift+k" = "move up";

            # Resize
            "${modifier}+z" = ''mode "resize"'';

            # Splitting
            "${modifier}+minus" = "split v";
            "${modifier}+backslash" = "split h";

            # Rofi
            "${modifier}+d" = ''exec "rofi -show drun"'';
            "${modifier}+Shift+d" = ''exec "rofi -show combi -combi-modi run,drun"'';
            "${modifier}+Shift+s" = ''exec "rofi -show ssh"'';
            "${modifier}+Tab" = ''exec "rofi -show window"'';

            # Rofimoji
            "${modifier}+Shift+e" = ''exec "${pkgs.rofimoji}/bin/rofimoji"'';

            # Restart/reload
            "${modifier}+r" = "reload";
            "${modifier}+Shift+r" = "restart";
          } // (
            if ! nixonCfg.enable then {} else {
              # Nixon
              "${modifier}+x" = "exec --no-startup-id ${nixonCfg.package}/bin/nixon run";
              "${modifier}+Shift+x" = "exec --no-startup-id ${nixonCfg.package}/bin/nixon project";
            }
          ) // (
            # Sans Plasma
            if cfg.plasma then {} else {
              # Locking
              "Control+Mod1+l" = ''mode "${lockMode}"'';

              # Audio controls
              "XF86AudioRaiseVolume" = "exec --no-startup-id ${amixer} set Master 1%+";
              "XF86AudioLowerVolume" = "exec --no-startup-id ${amixer} set Master 1%-";
              "XF86AudioMute"        = "exec --no-startup-id ${amixer} set Master toggle";
              "XF86AudioMicMute"     = "exec --no-startup-id ${amixer} set Capture toggle";

              # Brightness
              # "XF86MonBrightnessUp"   = "exec ${backlight} -A 10";
              # "XF86MonBrightnessDown" = "exec ${backlight} -U 10";
            }
          )
        );
        modes = lib.mkMerge [
          {
            "resize" = {
              h = "resize shrink width 10 px or 10 ppt";
              j = "resize grow height 10 px or 10 ppt";
              k = "resize shrink height 10 px or 10 ppt";
              l = "resize grow width 10 px or 10 ppt";
              Return = "mode default";
              Escape = "mode default";
            };
          }
          (lib.mkIf (!cfg.plasma) {
            "${lockMode}" = {
              l = ''exec --no-startup-id "${lockCmd}", mode default'';
              e = ''exec --no-startup-id "${exitCmd}", mode default'';
              h = ''exec --no-startup-id "${hibernateCmd}", mode default'';
              s = ''exec --no-startup-id "${suspendCmd}", mode default'';
              r = ''exec --no-startup-id "${rebootCmd}", mode default'';
              p = ''exec --no-startup-id "${shutdownCmd}", mode default'';
              Return = "mode default";
              Escape = "mode default";
            };
          })
        ];
        startup = [
          { command = "~/.fehbg"; notification = false; }
        ] ++ (if (cfg.plasma) then [
          { command = "${pkgs.wmctrl} -c Plasma"; notification = false; }
          { command = "systemctl --user restart davmail.service"; notification = false; }
          { command = "systemctl --user restart i3ws.service"; always = true; notification = false; }
          { command = "systemctl --user restart picom.service"; notification = false; }
          { command = "systemctl --user restart polybar.service"; always = true; notification = false; }
          { command = "systemctl --user restart syncthing.service"; notification = false; }
          { command = "systemctl --user restart qsyncthingtray.service"; always = true; notification = false; }
        ] else [
          # Enable natural scrolling
          # { command = ''xinput set-prop "GDX1301:00 27C6:01F0 Touchpad" "libinput Natural Scrolling Enabled" 1''; notification = false; }
          { command = ''setxkbmap us alt-intl-unicode''; notification = false; }
          # { command = "systemctl --user restart davmail.service"; notification = false; }
          # { command = "systemctl --user restart syncthing.service"; notification = false; }
          # { command = "systemctl --user restart picom.service"; notification = false; }
          { command = "systemctl --user restart polybar.service"; always = true; notification = false; }
          { command = "systemctl --user restart i3ws.service"; always = true; notification = false; }
          { command = "sleep 2; systemctl --user restart qsyncthingtray.service"; always = true; notification = false; }
        ]);
        window = {
          # Disable title bar
          titlebar = false;
          commands = if (!cfg.plasma) then [] else [
            # Kill Plasma desktop
            { criteria = { title = "Desktop — Plasma"; }; command = "kill; floating enable; border none"; }

            # Plasma popus shouldn't be tiled
            { criteria = { class = "plasmashell"; };      command = "floating enable"; }
            { criteria = { class = "Plasma"; };           command = "floating enable; border none"; }
            { criteria = { title = "plasma-desktop"; };   command = "floating enable; border none"; }
            { criteria = { title = "win7"; };             command = "floating enable; border none"; }
            { criteria = { class = "krunner"; };          command = "floating enable; border none"; }
            { criteria = { class = "Kmix"; };             command = "floating enable; border none"; }
            { criteria = { class = "Klipper"; };          command = "floating enable; border none"; }
            { criteria = { class = "Plasmoidviewer"; };   command = "floating enable; border none"; }
          ];
        };
      };
    };
  };
}
