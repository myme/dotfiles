# Global system configuration
{ config, lib, pkgs, system, ... }: {
  imports = [ ./sleep.nix ./xserver.nix ./users.nix ];

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
        package = pkgs.nixUnstable;
        extraOptions = "experimental-features = nix-command flakes";
        settings = {
          trusted-users = [ "root" config.myme.machine.user.name ];
          trusted-public-keys =
            [ "Tuple:FRwemNd0zmxD24+XgQRibsfNH5Vl32rOdc0NWvtJLYE=" ];
        };
      };

      # Defaults/compat from "22.11"
      system.stateVersion = "22.11";
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
        # nativeSystemd = true;
        startMenuLaunchers = false; # Done below to include Home Manager apps
        wslConf.automount.root = "/mnt";
      };

      # Copied from https://github.com/nix-community/NixOS-WSL/blob/69783cf56b2ada7e0e8cc8d17907a346e8bd97b7/modules/wsl-distro.nix#L111
      system.activationScripts.copy-home-launchers = lib.stringAfter [ ] ''
        cd "$(mktemp -d)"
        for x in applications icons; do
          echo "Copying /usr/share/$x"
          ${pkgs.rsync}/bin/rsync -ar $systemConfig/sw/share/$x/. ./$x
          ${pkgs.rsync}/bin/rsync -ar /etc/profiles/per-user/${config.myme.machine.user.name}/share/$x/. ./$x
          mkdir -p /usr/share/$x
          ${pkgs.rsync}/bin/rsync -ar --delete ./$x/. /usr/share/$x
        done
      '';
    })
    (lib.mkIf (config.myme.machine.role != "server") {
      # For GTK stuff
      programs.dconf.enable = true;

      # Enable sound.
      sound.enable = true;
      hardware.pulseaudio.enable = true;
    })
    # Laptop configs
    (lib.mkIf (config.myme.machine.role == "laptop") {
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
