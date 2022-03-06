#
# Tuple is a Windows 11 machine and this configuration is for WSL on that host.
#
# Graphical apps are supported, but unfortunately not GL, see:
#
#   https://github.com/guibou/nixGL/issues/69
#

{
  myme.machine = {
    role = "desktop";
    genericLinux = true;
    highDPI = true;
    user = {
      name = "myme";

      # This maps to the `users.users.myme` NixOS config
      config = {
        isNormalUser = true;
        initialPassword = "nixos";
        extraGroups = [ "wheel" "networkmanager" ];
        openssh.authorizedKeys.keys = [];
      };

      # This maps to the `home-manager.users.myme` NixOS (HM module) config
      profile = {
        imports = [
          ../home-manager
        ];

        config = {
          home.sessionVariables.LANG = "en_US.UTF-8";

          programs = {
            home-manager.enable = true;

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
    };
  };
}
