# Global system configuration
{ config, lib, pkgs, options, system, ... }: {
  imports = [ ./sleep.nix ./xserver.nix ./users.nix ./docker-desktop-fix.nix ];

  options.myme.machine = {
    name = lib.mkOption {
      type = lib.types.str;
      default = "nixos";
      description = "Machine name";
    };
    stable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Use stable NixOS release";
    };
    role = lib.mkOption {
      type = lib.types.enum [ "desktop" "laptop" "server" ];
      default = "desktop";
      description = "Machine type";
    };
    flavor = lib.mkOption {
      type = lib.types.enum [ "nixos" "generic" "wsl" ];
      default = "nixos";
      description = "Linux flavor";
    };
  };

  config = lib.mkMerge [
    # Defaults
    {
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
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "no";
        };
      };

      # GnuPG - enable if enabled for any user
      programs.gnupg.agent = {
        enable = with builtins;
          any (x: x) (map (x: x.services.gpg-agent.enable)
            (attrValues config.home-manager.users));
        enableSSHSupport = true;
      };

      # Nix
      nix = {
        package = if pkgs.nixVersions ? "latest" then pkgs.nixVersions.latest else pkgs.nixUnstable;
        extraOptions = "experimental-features = nix-command flakes";
        nixPath = ["nixpkgs=flake:nixpkgs"];
        settings = {
          trusted-users = [ "root" config.myme.machine.user.name ];
          trusted-public-keys =
            [ "Tuple:FRwemNd0zmxD24+XgQRibsfNH5Vl32rOdc0NWvtJLYE=" ];
        };
      };

      # build-vm configs
      virtualisation.vmVariant.virtualisation = {
        forwardPorts = [{
          from = "host";
          host.port = 2222;
          guest.port = 22;
        }];
      };

      system.stateVersion = "24.05";
    }
    # Disable boot + networking for WSL
    (lib.mkIf (config.myme.machine.flavor != "wsl") {
      # Boot
      boot.loader.systemd-boot.enable = lib.mkDefault true;
      boot.loader.systemd-boot.configurationLimit = lib.mkDefault 30;
      boot.loader.efi.canTouchEfiVariables = lib.mkDefault true;
      boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;

      # Network
      # networking.hostName = config.myme.machine.name;
      networking.networkmanager.enable = true;
      networking.firewall.enable = true;
    })
    # Enable NixOS-WSL module
    (lib.mkIf (config.myme.machine.flavor == "wsl") {
      wsl = let username = config.myme.machine.user.name;
      in {
        enable = true;
        defaultUser = username;
        interop.register = true;
        startMenuLaunchers = true;
        docker-desktop.enable = true;
      };
      fix.docker-desktop.enable = true;
    })
    (lib.mkIf (config.myme.machine.role != "server") {
      # For GTK stuff
      programs.dconf.enable = true;

      # Enable sound.
      # TODO: Reduce to services.pulseaudio.enable = true; when NixOS stable is updated
      ${if options.services ? pulseaudio then "services" else "hardware"}.pulseaudio.enable = true;
    })
    # Laptop configs
    (lib.mkIf (config.myme.machine.role == "laptop") {
      # Bluetooth
      hardware.bluetooth.enable = true;
      services.blueman.enable = true;
    })
  ];
}
