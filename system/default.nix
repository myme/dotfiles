# Global system configuration
{ pkgs, ... }: {
  imports = [
    ./hardware.nix
  ];

  # Boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Time
  time.timeZone = "Europe/Oslo";

  # System packages
  environment.systemPackages = with pkgs; [ vim ];

  # For GTK stuff
  programs.dconf.enable = true;

  # SSH
  services.openssh.enable = true;

  # Nix
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = "experimental-features = nix-command flakes";
}
