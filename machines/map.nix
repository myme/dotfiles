#
# `map` is a Windows 11 machine and this configuration is for WSL on that host.
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
      highDPI = false;
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

            home.sessionVariables = {
              # TODO: Move this into home-manager config
              EDITOR = "et";
              # TODO: Base this on WSL config
              XDG_RUNTIME_DIR = "/run/user/$(id -u)";
            };

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

            myme.dev = {
              docs.enable = true;
              nodejs.enable = true;
              haskell.enable = true;
            };

            services = {
              syncthing.enable = true;
            };
          };
        };
      };
    };

    age.secrets.ssh = {
      file = ./../secrets/ssh.age;
      owner = config.myme.machine.user.name;
    };

    documentation.nixos.enable = true;
  };
 }
