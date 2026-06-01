{ config, lib, ... }:

let
  cfg = config.myme.machine.sleep;

in
{
  options.myme.machine.sleep = {
    hibernateDelay = lib.mkOption {
      type = lib.types.str;
      default = "24h";
      description = "Timeout for hibernating the system from sleep.";
    };
  };

  # WSL skips the standard systemd module, so the option isn't declared.
  config = lib.mkIf (config.myme.machine.flavor != "wsl") {
    systemd.sleep.settings.Sleep.HibernateDelaySec = cfg.hibernateDelay;
  };
}
