# Global system configuration
{ pkgs, ... }: {
  imports = [
    ./hardware.nix
  ];

  # Boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 30;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Network
  networking.networkmanager.enable = true;
  networking.firewall.enable = true;

  # Time
  time.timeZone = "Europe/Oslo";

  # System packages
  environment.systemPackages = with pkgs; [ vim ];

  # For GTK stuff
  programs.dconf.enable = true;

  # Mosh
  programs.mosh.enable = true;

  # SSH
  services.openssh.enable = true;

  # Nix
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = "experimental-features = nix-command flakes";
}
