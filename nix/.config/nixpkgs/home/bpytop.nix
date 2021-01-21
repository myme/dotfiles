{ config, lib, pkgs, ... }:

let
  package = pkgs.bpytop;
  desktopItem = pkgs.writeTextDir "share/applications/bpytop.desktop"
    (lib.generators.toINI { } {
      "Desktop Entry" = {
        Type = "Application";
        Version = package.version;
        Exec = "${package}/bin/bpytop";
        Terminal = true;
        Name = "bpytop";
        # Icon = "bpytop";
        Comment = "Linux/OSX/FreeBSD resource monitor ";
        GenericName = "Resource Monitor";
        Categories = "System;Monitor;ConsoleOnly;";
      };
    });

in {
  config = {
    home.packages = [
      package
      desktopItem
    ];
  };
}
