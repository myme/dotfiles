{
  config,
  options,
  lib,
  ...
}:

let
  cfg = config.myme.machine.sleep;
  # `systemd.sleep.extraConfig` was removed in nixpkgs-unstable in favor
  # of `systemd.sleep.settings.Sleep`. Stable (25.11) still has only the
  # old option. Pick whichever exists in the current nixpkgs so this
  # module evaluates on both. Once stable catches up, drop the `else`.
  newOption = options.systemd.sleep ? settings;

in
{
  options.myme.machine.sleep = {
    hibernateDelay = lib.mkOption {
      type = lib.types.str;
      default = "24h";
      description = "Timeout for hibernating the system from sleep.";
    };
  };

  # WSL skips the standard systemd module, so neither option is declared.
  config = lib.mkIf (config.myme.machine.flavor != "wsl") (
    if newOption then
      { systemd.sleep.settings.Sleep.HibernateDelaySec = cfg.hibernateDelay; }
    else
      {
        systemd.sleep.extraConfig = ''
          HibernateDelaySec=${cfg.hibernateDelay}
        '';
      }
  );
}
