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
          openssh.authorizedKeys.keys = [ ];
        };

        # This maps to the `home-manager.users.myme` NixOS (HM module) config
        profile = {
          imports = [ ../../home-manager ];

          config = {
            home.packages = with pkgs; [ mosh ];

            programs = {
              # SSH agent
              keychain = {
                enable = true;
                keys = [ "id_ed25519" ];
                extraFlags = [ "--quiet" "--systemd" ];
              };

              ssh = {
                enable = true;
                includes = [ config.age.secrets.ssh.path ];
              };
            };

            myme.dev = {
              docs.enable = true;
              nodejs.enable = true;
              shell.enable = true;
            };

            myme.emacs.configExtra = ''
              (add-to-list 'auth-sources "${config.age.secrets.authinfo.path}" t)
            '';

            services = { syncthing.enable = true; };
          };
        };
      };
    };

    age.secrets = {
      authinfo = {
        file = ./authinfo.age;
        owner = config.myme.machine.user.name;
      };
      ssh = {
        file = ../../secrets/ssh.age;
        owner = config.myme.machine.user.name;
      };
    };

    documentation.nixos.enable = true;
    virtualisation.podman.enable = true;
  };
}
