{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.barrier;

in {
  options.services.barrier = {
    enable = mkEnableOption "Enable barrier integration";

    package = mkOption {
      type = types.package;
      default = pkgs.barrier;
      description = "barrier package";
    };
  };

  config = {
    systemd.user.services.barrier = mkIf cfg.enable {
      Unit = {
        Description = "barrier - Software KVM";
        Documentation = "https://github.com/debauchee/barrier";
      };

      Service = {
        ExecStart = "${cfg.package}/bin/barrier";
        Restart = "on-failure";
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
