{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # Apps
    firefox
    chromium
    gimp
    vlc

    # Utils
    peek
    rofimoji

    # Gnome
    gnome3.gnome-calculator
    gnome3.nautilus
    gnome3.evince
    gnome3.seahorse
  ];
}
