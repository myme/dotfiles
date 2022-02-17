# Global system configuration
{ config, lib, pkgs, ... }: {
  imports = [
    ./hardware.nix
    ./xserver.nix
    ./users.nix
  ];

  options.myme.machine = {
    name = lib.mkOption {
      type = lib.types.str;
      default = "nixos";
      description = "Machine name";
    };
    role = lib.mkOption {
      type = lib.types.enum [ "desktop" "laptop" "server" ];
      default = "desktop";
      description = "Machine type";
    };
    genericLinux = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "For non-NixOS hosts";
    };
  };

  config = lib.mkMerge [
    {
      # Boot
      boot.loader.systemd-boot.enable = true;
      boot.loader.systemd-boot.configurationLimit = 30;
      boot.loader.efi.canTouchEfiVariables = true;
      boot.kernelPackages = pkgs.linuxPackages_latest;

      # Network
      # networking.hostName = config.myme.machine.name;
      networking.networkmanager.enable = true;
      networking.firewall.enable = true;

      # Man
      documentation.man = {
        enable = true;
        generateCaches = true;
      };

      # Time
      time.timeZone = "Europe/Oslo";

      # System packages
      environment.systemPackages = with pkgs; [ vim ];

      # Mosh
      programs.mosh.enable = true;

      # SSH
      services.openssh.enable = true;

      # GnuPG - enable if enabled for any user
      programs.gnupg.agent = {
        enable = with builtins;
          any (x: x)
            (map (x: x.services.gpg-agent.enable) (attrValues config.home-manager.users));
        enableSSHSupport = true;
      };

      # Nix
      nix.package = pkgs.nixUnstable;
      nix.extraOptions = "experimental-features = nix-command flakes";
    }
    (lib.mkIf (config.myme.machine.role != "server") {
      # For GTK stuff
      programs.dconf.enable = true;

      # Enable sound.
      sound.enable = true;
      hardware.pulseaudio.enable = true;
    })
    # Laptop configs
    (lib.mkIf (config.myme.machine.role == "laptop") {
      # Backlight
      services.illum.enable = true;

      # Bluetooth
      hardware.bluetooth.enable = true;
      services.blueman.enable = true;

      # Media keys
      sound.mediaKeys = {
        enable = true;
        volumeStep = "1%";
      };
    })
  ];
}
