{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.myme.ssh;

  # Bare names are resolved against ~/.ssh, like keychain does. Spelled out in
  # full because the unit runs these through `exec`, not a shell, so there is
  # nothing around to expand a `~`.
  keyPath = key: if lib.hasPrefix "/" key then key else "${config.home.homeDirectory}/.ssh/${key}";

in
{
  options.myme.ssh = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Manage the SSH client config and run an agent alongside it.";
    };

    agentKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "id_ed25519" ];
      description = ''
        Private keys to load into the SSH agent when the user session starts.
        Only useful for passphrase-less keys: the unit runs without a terminal,
        so a passphrase prompt has nowhere to go and the load simply fails.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # Let systemd own the agent rather than the first login shell. The socket
    # then lives at a fixed `$XDG_RUNTIME_DIR/ssh-agent`, and home-manager
    # publishes SSH_AUTH_SOCK to the systemd/D-Bus user environment as well as
    # to shells. That second half is what children started outside a shell
    # need, the Emacs daemon above all: without it, git commit signing from
    # magit dies on `Couldn't get agent socket?`.
    #
    # Mutually exclusive with keychain on purpose. Keychain only adopts an
    # existing agent when *both* SSH_AUTH_SOCK and SSH_AGENT_PID are set, and
    # systemd exports only the former, so enabling both just means two agents
    # racing over which one the shell points at -- with the keys landing in
    # the one Emacs cannot see.
    services.ssh-agent.enable = lib.mkDefault (!config.programs.keychain.enable);

    systemd.user.services.ssh-add-keys = lib.mkIf (cfg.agentKeys != [ ]) {
      Unit = {
        Description = "Add SSH keys to the agent";
        Requires = [ "ssh-agent.service" ];
        After = [ "ssh-agent.service" ];
        # A restarted agent is an empty agent, so come back up with it.
        PartOf = [ "ssh-agent.service" ];
      };
      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        # ssh-agent is a plain Type=simple unit, so systemd calls it active as
        # soon as it forks -- possibly a beat before the socket is bound.
        ExecStart = pkgs.writeShellScript "ssh-add-keys" ''
          for _ in $(seq 50); do
            [ -S "$SSH_AUTH_SOCK" ] && break
            sleep 0.1
          done
          exec ${lib.getExe' pkgs.openssh "ssh-add"} ${lib.escapeShellArgs (map keyPath cfg.agentKeys)}
        '';
      };
      Install.WantedBy = [ "default.target" ];
    };

    programs.ssh = {
      enable = true;
      includes = [
        # Allow adding custom, mutable, per-host configs
        "~/.ssh/hosts"
      ];
      # Opt out of the soon-to-be removed implicit defaults, keeping the
      # previous values explicitly so ~/.ssh/config is unchanged.
      # See:
      #  - https://github.com/nix-community/home-manager/pull/7655
      #  - https://github.com/nix-community/home-manager/pull/7737
      enableDefaultConfig = false;
      settings."*" = {
        forwardAgent = false;
        addKeysToAgent = "no";
        compression = false;
        serverAliveInterval = 0;
        serverAliveCountMax = 3;
        hashKnownHosts = false;
        userKnownHostsFile = "~/.ssh/known_hosts";
        controlMaster = "no";
        controlPath = "~/.ssh/master-%r@%n:%p";
        controlPersist = "no";
      };
    };
  };
}
