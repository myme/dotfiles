# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 30;
  boot.loader.efi.canTouchEfiVariables = true;

  # LVM
  boot.initrd.luks.devices.root = {
    device = "/dev/disk/by-uuid/9fef8f27-eb30-4063-a8e0-29dff8b0e20f";
    preLVM = true;
  };

  # Kernel
  # boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "set"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # VirtualBox
  # virtualisation.virtualbox.host.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    w3m
    wget
  ];

  # ZSH
  programs.zsh.enable = true;

  # Backlight
  # services.illum.enable = true;

  # Bluetooth
  # hardware.bluetooth.enable = true;
  # services.blueman.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  programs.dconf.enable = true;

  # gnome-keyring - https://github.com/jluttine/NiDE/blob/master/src/keyring.nix
  services.gnome3.gnome-keyring.enable = true;
  security.pam.services.xdm.enableGnomeKeyring = true;
  # environment.variables.SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/keyring/ssh";
  # programs.ssh.askPassword = "${pkgs.gnome3.seahorse}/libexec/seahorse/ssh-askpass";

  # List services that you want to enable:
  services.dbus.packages = [];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Man
  documentation.man = {
    enable = true;
    generateCaches = true;
  };

  # Cron
  # services.cron = {
  #   enable = true;
  #   systemCronJobs = [
  #   ];
  # };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "alt-intl-unicode";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.desktopManager.session = [
    {
      name = "home-manager";
      start = ''
        ${pkgs.runtimeShell} $HOME/.hm-xsession &
        waitPID=$!
      '';
    }
  ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };

  users.users.mmyrseth = {
    isNormalUser = true;
    home = "/home/mmyrseth";
    description = "Martin Øinæs Myrseth";
    extraGroups = [ "wheel" "networkmanager" "vboxusers" ];
    shell = pkgs.zsh;
  };

  # Allow unfree software
  nixpkgs.config.allowUnfree = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.09"; # Did you read the comment?

}
