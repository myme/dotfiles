# Global system configuration
{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./machine.nix
    ./sleep.nix
    ./xserver.nix
    ./users.nix
  ];

  config = lib.mkMerge [
    # Defaults
    {
      # Man
      documentation.man = {
        enable = true;
        generateCaches = true;
      };

      # Time
      time.timeZone = lib.mkDefault "Europe/Oslo";
      services.automatic-timezoned.enable = true;

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
        enable =
          with builtins;
          any (x: x) (map (x: x.services.gpg-agent.enable) (attrValues config.home-manager.users));
        enableSSHSupport = true;
      };

      # Nix
      nix = {
        package = if pkgs.nixVersions ? "latest" then pkgs.nixVersions.latest else pkgs.nixUnstable;
        extraOptions = "experimental-features = nix-command flakes";
        nixPath = [ "nixpkgs=flake:nixpkgs" ];
        settings = {
          trusted-users = [
            "root"
            config.myme.machine.user.name
          ];
          trusted-public-keys = [ "Tuple:FRwemNd0zmxD24+XgQRibsfNH5Vl32rOdc0NWvtJLYE=" ];
        };
      };

      # build-vm configs
      virtualisation.vmVariant.virtualisation = {
        forwardPorts = [
          {
            from = "host";
            host.port = 2222;
            guest.port = 22;
          }
        ];
      };

      system.stateVersion = "24.05";

      # On NixOS-WSL `/etc/resolv.conf` is provided by WSL; on other flavors
      # NetworkManager manages it. In both cases the default resolvconf
      # service would conflict with `environment.etc."resolv.conf"`.
      networking.resolvconf.enable = false;
    }
    # Disable boot + networking for WSL
    (lib.mkIf (config.myme.machine.flavor != "wsl") {
      # Boot
      boot.loader = {
        systemd-boot.enable = lib.mkDefault true;
        systemd-boot.configurationLimit = lib.mkDefault 30;
        efi.canTouchEfiVariables = lib.mkDefault true;
      };
      # boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;

      # Network
      # networking.hostName = config.myme.machine.name;
      networking = {
        networkmanager.enable = true;
        firewall.enable = true;
      };
    })
    # Enable NixOS-WSL module
    (lib.mkIf (config.myme.machine.flavor == "wsl") {
      # automatic-timezoned uses geoclue which doesn't work in WSL; disable it
      # so the static time.timeZone default is used instead.
      services.automatic-timezoned.enable = lib.mkForce false;

      wsl =
        let
          username = config.myme.machine.user.name;
        in
        {
          enable = true;
          defaultUser = username;
          interop.register = true;
          startMenuLaunchers = true;
          # docker-desktop.enable = true;
        };
    })
    (lib.mkIf (config.myme.machine.role != "server") {
      # For GTK stuff
      programs.dconf.enable = true;
      # Enable sound.
      services.pipewire.enable = true;
    })
    # Laptop configs
    (lib.mkIf (config.myme.machine.role == "laptop") {
      # Bluetooth
      hardware.bluetooth.enable = true;
      services.blueman.enable = true;
    })
  ];
}
