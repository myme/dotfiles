# Windows Subsystem for Linux (WSL)
#
# Primarily for WSL non-GUI user environment.
#

{
  home-manager.users.myme = {
    imports = [
      ../home-manager
    ];

    config = {
      home.sessionVariables.LANG = "en_US.UTF-8";

      # Non-NixOS
      targets.genericLinux.enable = true;

      programs = {
        home-manager.enable = true;

        bash.profileExtra = ''
          if [ -e /home/myme/.nix-profile/etc/profile.d/nix.sh ]; then
            . /home/myme/.nix-profile/etc/profile.d/nix.sh;
          fi
        '';

        # SSH agent
        keychain = {
          enable = true;
          keys = [ "id_ed25519" ];
        };

        tmux.secureSocket = false;
      };

      # Enable flakes
      xdg.configFile."nix/nix.conf".text = ''
        experimental-features = nix-command flakes
      '';
    };
  };
}

