{ config, lib, pkgs, ... }:

with lib;

{
  config = {
    home.packages = [ pkgs.davmail ];

    systemd.user.services.davmail = {
      Unit = {
        Description = "Davmail Exchange IMAP Proxy";
        After = "graphical-session-pre.target";
        PartOf = "graphical-session.target";
      };

      Service = {
        Environment = "PATH=${pkgs.davmail}/bin:${pkgs.coreutils}/bin";
        ExecStart = "${pkgs.davmail}/bin/davmail";
        Restart = "on-failure";
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
