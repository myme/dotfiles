{ overlays, system }: {
  inherit system;
  homeDirectory = "/home/myme";
  username = "myme";
  configuration = (import ../home-manager {
    nixpkgs.overlays = overlays;
    home.sessionVariables.LANG = "en_US.UTF-8";

    # Non-NixOS
    targets.genericLinux.enable = true;

    # programs = attrs.programs // {
    programs = {
      home-manager.enable = true;

      bash.profileExtra = ''
        if [ -e /home/myme/.nix-profile/etc/profile.d/nix.sh ]; then
          . /home/myme/.nix-profile/etc/profile.d/nix.sh;
        fi
      '';

      tmux.secureSocket = false;
    };

    # Enable flakes
    xdg.configFile."nix/nix.conf".text = ''
      experimental-features = nix-command flakes
    '';
  });
}

