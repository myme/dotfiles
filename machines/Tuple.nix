#
# `Tuple` is a Windows 11 machine and this configuration is for WSL on that host.
#
# Graphical apps are supported, but unfortunately not GL, see:
#
#   https://github.com/guibou/nixGL/issues/69
#

{
  system = "x86_64-linux";
  config = { config, pkgs, ... }: {
    myme.machine = {
      role = "desktop";
      flavor = "wsl";
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
            home.packages = with pkgs; [
              mosh
            ];

            programs = {
              # SSH agent
              keychain = {
                enable = true;
                keys = [ "id_ed25519" ];
              };

              ssh = {
                enable = true;
                includes = [
                  config.age.secrets.ssh.path
                ];
              };

            };

            services = {
              syncthing.enable = true;
            };

            myme.dev = {
              haskell.enable = true;
              nodejs.enable = true;
            };
          };
        };
      };
    };

    boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

    age.secrets.ssh = {
      file = ./../secrets/ssh.age;
      owner = config.myme.machine.user.name;
    };

    networking.firewall.enable = false;

    virtualisation.podman.enable = true;
  };
}
