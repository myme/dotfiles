{ config, lib, pkgs, ... }: {
  myme.machine = {
    role = "server";
    user = {
      name = "myme";
      config = {
        isNormalUser = true;
        initialPassword = "nixos";
        extraGroups = [ "libvirtd" "wheel" ]; # Enable ‘sudo’ for the user.
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKff0OXZApWIawdc6tymlGjaBXvPKMt4UwPcGF12w3Wz myme@stack"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH+9tnNlMesGrK/lDvycgzyS4pPrsGqcGQP6yLCsr/LN myme@Tuple"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII1Qsv8MA+cyu7n+4H1kpbVrAmOosJJxjPWAdl08YDvL myme@map"
        ];
      };
      profile = {
        imports = [
          ../../home-manager
        ];

        home.sessionVariables = {
          EDITOR = "et";
        };

        myme.irc = {
          enable = true;
          service = true;
        };

        programs = {
          ssh = {
            enable = true;
            includes = [
              config.age.secrets.ssh.path
            ];
          };

        };
      };
    };
  };

  nix.settings.trusted-public-keys = [
    "tuple:RLwVT0X7XUres7PkgkMLgsMfWhbHP0PYIfQmqJ2M6Ac="
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/663eb7a2-61a1-497e-8e25-2a9138fbe41c";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/60EC-0B67";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/a7610703-0458-4de9-92dd-229d69dc8936"; }
    ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  # high-resolution display
  hardware.video.hidpi.enable = lib.mkDefault true;


  networking = {
    defaultGateway = "192.168.1.1";
    nameservers = [ "8.8.8.8" ];
    bridges.br0.interfaces = ["eno1"];
    interfaces = {
      # wlp0s20f3.useDHCP = true;
      br0 = {
        useDHCP = false;
        ipv4.addresses = [{
          "address" = "192.168.1.5";
          "prefixLength" = 24;
        }];
      };
    };
    firewall.allowedTCPPorts = [
      8000
      8080
      8888
    ];
  };

  age.secrets = {
    ssh = {
      file = ./../../secrets/ssh.age;
      owner = config.myme.machine.user.name;
    };
    weechat = {
      file = ./weechat.age;
      owner = config.myme.machine.user.name;
    };
  };

  environment.systemPackages = with pkgs; [
    virt-manager
  ];

  services.tailscale.enable = true;

  # Virtualization
  virtualisation = {
    libvirtd = {
      enable = true;
      qemu.ovmf.enable = true;
    };
    podman.enable = true;
  };
}
