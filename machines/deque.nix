let system = "x86_64-linux";
in {
  inherit system;
  deployTo = "deque.myme.no";
  config = { config, lib, modulesPath, pkgs, ... }: {
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
          imports = [ ../home-manager ];
          programs.emacs.enable = false;
          home.sessionVariables = { EDITOR = "et"; };
        };
      };
    };

    # Security
    security = {
      sudo.execWheelOnly = true;
      pam = {
        enableSSHAgentAuth = true;
        services.sudo.sshAgentAuth = true;
      };
    };

    # SSH
    services.openssh = {
      allowSFTP = false;
      passwordAuthentication = false;
      ports = [ 22345 ];
      permitRootLogin = "no";
      startWhenNeeded = true;
    };

    # Boot
    boot.loader.grub.enable = true;
    boot.loader.grub.version = 2;
    boot.loader.grub.devices = [ "/dev/sda" ];

    # Hardware configuration
    imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

    boot.initrd.availableKernelModules =
      [ "ahci" "xhci_pci" "virtio_pci" "sd_mod" "sr_mod" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ ];
    boot.extraModulePackages = [ ];

    fileSystems."/" = {
      device = "/dev/sda1";
      fsType = "ext4";
    };

    swapDevices = [ ];

    networking.useDHCP = lib.mkDefault true;

    nixpkgs.hostPlatform = lib.mkDefault system;
    hardware.cpu.amd.updateMicrocode =
      lib.mkDefault config.hardware.enableRedistributableFirmware;

    boot.loader.systemd-boot.enable = false;
    boot.kernelPackages = pkgs.linuxPackages;
  };
}
