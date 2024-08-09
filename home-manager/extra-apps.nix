{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # Apps
    firefox
    chromium
    gimp
    vlc

    # Utils
    lnav
    peek
    rofimoji

    # Gnome
    gnome-calculator
    nautilus
    evince
    seahorse
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
