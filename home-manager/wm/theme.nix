{ lib, pkgs, ... }@args:

let
  machine = args.specialArgs.nixosConfig.myme.machine;

  iconThemes = {
    numix-circle = {
      package = pkgs.numix-icon-theme;
      name = "Numix";
    };
  };
  cursorThemes = {
    capitaine = {
      package = pkgs.capitaine-cursors;
      name = "capitaine-cursors";
      size = if machine.highDPI then 64 else 50;
    };
    none = {};
    numix = {
      package = pkgs.numix-cursor-theme;
      name = "Numix-Cursor";
    };
  };
  themes = {
    adwaita = {
      package = pkgs.gnome3.gnome_themes_standard;
      # name = "Adwaita";
      name = "Adwaita-dark";
    };
    dracula = {
      package = pkgs.dracula-theme;
      name = "Dracula";
    };
    nordic = {
      package = pkgs.nordic;
      name = "Nordic";
    };
  };

in {
  config = lib.mkIf (machine.role != "server") {
    gtk = {
      enable = true;
      iconTheme = iconThemes.numix-circle;
      theme = themes.dracula;
    };
    home.pointerCursor = {
      gtk.enable = true;
      x11.enable = true;
    } // cursorThemes.capitaine;
  };
}
