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

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = ["firefox.desktop"];
      "text/xml" = ["firefox.desktop"];
      "application/xhtml+xml" = ["firefox.desktop"];
      "application/vnd.mozilla.xul+xml" = ["firefox.desktop"];
      "x-scheme-handler/http" = ["firefox.desktop"];
      "x-scheme-handler/https" = ["firefox.desktop"];
      "x-scheme-handler/ftp" = ["firefox.desktop"];
    };
  };
}
