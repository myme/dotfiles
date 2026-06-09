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

  # WSL doesn't load the standard systemd module, so `systemd.sleep.settings`
  # isn't declared there.
  config = lib.mkIf (config.myme.machine.flavor != "wsl") {
    systemd.sleep.settings.Sleep.HibernateDelaySec = cfg.hibernateDelay;
  };
}
