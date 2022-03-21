{ config, lib, ... }:

let
  cfg = config.myme.machine.sleep;

in {
  options.myme.machine.sleep = {
    hibernateDelay = lib.mkOption {
      type = lib.types.str;
      default = "24h";
      description = "Timeout for hibernating the system from sleep.";
    };
  };

  config = {
    systemd.sleep.extraConfig = ''
      HibernateDelaySec=${cfg.hibernateDelay}
    '';
  };
}
