{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.myme.i3ws;
  i3pkg = config.xsession.windowManager.i3.package;
  i3cfg = config.xsession.windowManager.i3.config;
  iconFlag = if cfg.icons then "-i" else "";
  separator = ''-s "${cfg.separator}"'';
  renumberFlag = "--no-renumber";
  cmd = "${cfg.package}/bin/i3ws ${iconFlag} ${separator} ${renumberFlag}";

in {
  options.myme.i3ws = {
    enable = mkEnableOption "Enable i3ws integration";

    package = mkOption {
      type = types.package;
      default = pkgs.haskellPackages.i3ws;
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
        ExecStart = ''${cmd} monitor'';
        Restart = "on-failure";
      };

      Install = {
        WantedBy = [ "default.target" ];
      };
    };

    # i3 keybindings
    xsession.windowManager.i3.config.keybindings = mkOptionDefault {
      "${i3cfg.modifier}+c" = "exec --no-startup-id ${cmd} new";
      "${i3cfg.modifier}+Shift+c" = "exec --no-startup-id ${cmd} move new";
      "${i3cfg.modifier}+Shift+p" = "exec --no-startup-id ${cmd} move left";
      "${i3cfg.modifier}+Shift+n" = "exec --no-startup-id ${cmd} move right";
    };
  };
}
