{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.myme.tailnetMosh;
in
{
  options.myme.tailnetMosh = {
    enable = lib.mkEnableOption "Mosh with SSH forwarded through Tailscale Serve";

    user = lib.mkOption {
      type = lib.types.str;
      description = "Local user allowed to log in through the tailnet SSH listener.";
    };

    ipv4 = lib.mkOption {
      type = lib.types.strMatching "[0-9]{1,3}(\\.[0-9]{1,3}){3}";
      description = "The Mac's Tailscale IPv4 address for Mosh UDP sessions.";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.mosh
      (lib.hiPrio (
        pkgs.writeShellScriptBin "mosh-server" ''
          # Serve terminates TCP on loopback. Give Mosh's -s option the
          # Tailscale IPv4 address where clients can reach its UDP socket.
          IFS=' ' read -r client_address client_port server_address server_port <<< "''${SSH_CONNECTION-}"
          if [[ "$server_address" == 127.0.0.1 && "$server_port" == 2222 ]]; then
            export SSH_CONNECTION="$client_address $client_port ${cfg.ipv4} $server_port"
          fi
          exec ${lib.getExe' pkgs.mosh "mosh-server"} "$@"
        ''
      ))
    ];

    # Tailscale Serve forwards tailnet TCP/22 to this loopback listener.
    services.openssh.enable = false;
    environment.etc."ssh/sshd_config_tailnet".text = ''
      ListenAddress 127.0.0.1:2222
      AllowUsers ${cfg.user}
      PermitRootLogin no
      AuthenticationMethods publickey
      PasswordAuthentication no
      KbdInteractiveAuthentication no
      AuthorizedKeysFile none
      Include /etc/ssh/sshd_config.d/099-host-keys.conf
      Include /etc/ssh/sshd_config.d/100-macos.conf
      Include /etc/ssh/sshd_config.d/101-authorized-keys.conf
      Subsystem sftp /usr/libexec/sftp-server
    '';
    launchd.daemons.tailnet-sshd = {
      command = "/usr/sbin/sshd -D -f /etc/ssh/sshd_config_tailnet";
      serviceConfig.KeepAlive = true;
    };
  };
}
