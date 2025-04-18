let
  system = "x86_64-linux";
  sshPort = 22345;
in {
  inherit system;
  deploy = {
    host = "deque.myme.no";
    sshOpts = [ "-A" "-p" (builtins.toString sshPort) ];
  };
  stable = true;
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
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIlMUotM7KE9qbVmLQbrp9+gvw8bwtrSU2aEYIG59saC myme@trie"
          ];
        };
        profile = {
          imports = [ ../home-manager ];
          # Avoid installing unnecessary and large programs.
          myme.defaultPrograms = false;
          home.packages = with pkgs; [
            btop
            dua
            fd
            jq
            lsof
            tree
          ];
          programs.fzf.enable = true;
        };
      };
    };

    # Network
    networking.firewall.allowedTCPPorts = [ 80 443 ];

    # Nginx
    services.nginx = {
      enable = true;
      upstreams = {
        rtcp.servers = { "127.0.0.1:8000" = { }; };
        stats.servers = { "127.0.0.1:8080" = { }; };
      };
      virtualHosts = {
        "rtcp.myme.no" = {
          enableACME = true;
          forceSSL = true;
          locations."/" = {
            proxyPass = "http://rtcp";
            proxyWebsockets = true;
          };
        };
        "stats.myme.no" = {
          enableACME = true;
          forceSSL = true;
          locations."/".proxyPass = "http://stats";
        };
      };
    };

    # ACME
    security.acme = {
      acceptTerms = true;
      defaults.email = "mm@myme.no";
    };

    # OCI
    virtualisation.docker.enable = true;

    # Security
    security = {
      sudo.execWheelOnly = true;
      pam = {
        sshAgentAuth.enable = true;
        services.sudo.sshAgentAuth = true;
      };
    };

    # SSH
    services.openssh = {
      allowSFTP = false;
      ports = [ sshPort ];
      settings.PasswordAuthentication = false;
      settings.PermitRootLogin = "no";
      startWhenNeeded = true;
    };

    # Boot
    boot.loader.grub.enable = true;
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
