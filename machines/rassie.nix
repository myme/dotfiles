let system = "aarch64-linux";
in {
  inherit system;
  deployTo = "192.168.1.7";
  config = { config, lib, modulesPath, pkgs, ... }: {
    # TODO: Fix native cross-compilation to aarch64
    # nixpkgs.buildPlatform = "x86_64-linux";
    # nixpkgs.hostPlatform = system;

    imports = [ "${modulesPath}/installer/sd-card/sd-image-aarch64.nix" ];
    sdImage.compressImage = false;

    myme.machine = {
      role = "server";
      user = {
        name = "myme";
        config = {
          isNormalUser = true;
          initialPassword = "nixos";
          extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
          openssh.authorizedKeys.keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII1Qsv8MA+cyu7n+4H1kpbVrAmOosJJxjPWAdl08YDvL myme@map"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKff0OXZApWIawdc6tymlGjaBXvPKMt4UwPcGF12w3Wz myme@stack"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH+9tnNlMesGrK/lDvycgzyS4pPrsGqcGQP6yLCsr/LN myme@Tuple"
          ];
        };
        profile = {
          imports = [
            ../home-manager
          ];
          home.sessionVariables = {
            EDITOR = "et";
          };
        };
      };
    };

    networking = {
      defaultGateway = "192.168.1.1";
      nameservers = [ "8.8.8.8" ];
      interfaces.eth0 = {
        useDHCP = false;
        ipv4.addresses = [{
          "address" = "192.168.1.7";
          "prefixLength" = 24;
        }];
      };
    };

    boot.loader.systemd-boot.enable = false;
    boot.kernelPackages = pkgs.linuxPackages;
  };
}
