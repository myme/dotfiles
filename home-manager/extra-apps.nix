{ pkgs, ... }:
{
  home.packages = [
    # Apps
    pkgs.firefox
    pkgs.chromium
    pkgs.gimp
    pkgs.vlc

    # Utils
    pkgs.lnav
    pkgs.peek
    pkgs.rofimoji

    # Gnome
    pkgs.gnome-calculator
    pkgs.nautilus
    pkgs.evince
    pkgs.file-roller
    pkgs.seahorse
  ];

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/markdown" = [ "emacsclient.desktop" ];
      "text/plain" = [ "emacsclient.desktop" ];
      "text/html" = [ "firefox.desktop" ];
      "text/xml" = [ "firefox.desktop" ];
      "application/xhtml+xml" = [ "firefox.desktop" ];
      "application/vnd.mozilla.xul+xml" = [ "firefox.desktop" ];
      "x-scheme-handler/http" = [ "firefox.desktop" ];
      "x-scheme-handler/https" = [ "firefox.desktop" ];
      "x-scheme-handler/ftp" = [ "firefox.desktop" ];
    };
  };
}
