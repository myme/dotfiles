{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.myme.i3ws;
  i3pkg = config.xsession.windowManager.i3.package;
  i3cfg = config.xsession.windowManager.i3.config;
  iconFlag = if cfg.icons then "-i" else "";
  separator = ''-s "${cfg.separator}"'';
  i3ws = "${pkgs.i3ws}/bin/i3ws ${iconFlag} ${separator}";

in {
  options.myme.i3ws = {
    enable = mkEnableOption "Enable i3ws integration";

    package = mkOption {
      type = types.package;
      default = (import (pkgs.fetchFromGitHub {
        owner = "myme";
        repo = "i3ws";
        rev = "c2b49862be3e969d6f310b823a6caed660f1d34f";
        sha256 = "1jr91w6is1wpl2zrl7i6cq39vszafz7p1km92mgqnnkjsajj437i";
      }) {}).i3ws;
      description = "i3ws package";
    };

    icons = mkOption {
      type = types.bool;
      default = false;
      description ="Enable i3ws integration";
    };

    separator = mkOption {
      type = types.str;
      default = ":";
      description ="Separator between workspace number and icons";
    };
  };

  config = {
    # i3ws service
    systemd.user.services.i3ws = mkIf cfg.enable {
      Unit = {
        Description = "i3ws - automatic i3 workspace manager";
        Documentation = "https://github.com/myme/i3ws";
      };

      Service = {
        Environment = "PATH=${i3pkg}/bin";
        ExecStart = ''${i3ws} monitor'';
        Restart = "on-failure";
      };

      Install = {
        WantedBy = [ "default.target" ];
      };
    };

    # i3 keybindings
    xsession.windowManager.i3.config.keybindings = mkOptionDefault {
      "${i3cfg.modifier}+c" = "exec --no-startup-id ${i3ws} new";
      "${i3cfg.modifier}+Shift+c" = "exec --no-startup-id ${i3ws} move new";
      "${i3cfg.modifier}+Shift+p" = "exec --no-startup-id ${i3ws} move left";
      "${i3cfg.modifier}+Shift+n" = "exec --no-startup-id ${i3ws} move right";
    };
  };
}
