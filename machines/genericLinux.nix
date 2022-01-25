# Generic Linux
#
# Intended for machines running non-NixOS (incomplete).
#
# Most likely needs NixGL to work nicely with OpenGL apps.
#

{ overlays, system }: {
  inherit system;
  homeDirectory = "/home/myme";
  username = "myme";
  configuration = {
    imports = [
      ../home-manager
    ];

    config = {
      nixpkgs.overlays = overlays;
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
      };

      # Enable flakes
      xdg.configFile."nix/nix.conf".text = ''
        experimental-features = nix-command flakes
      '';
    };
  };
}

