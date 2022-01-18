# Global system configuration
{ pkgs, ... }: {
  imports = [
    ./hardware.nix
    ./xserver.nix
  ];

  # Boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Network configuration.
  networking.useDHCP = false;
  networking.interfaces.ens3.useDHCP = true;
  # networking.interfaces.ens33.useDHCP = true;

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
