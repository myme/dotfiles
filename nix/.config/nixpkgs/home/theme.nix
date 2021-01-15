{ pkgs, ... }:

let
  iconThemes = {
    numix-circle = {
      package = pkgs.numix-icon-theme;
      name = "Numix";
    };
  };
  cursorThemes = {
    none = null;
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
      package = pkgs.ant-dracula-theme;
      name = "Ant-Dracula";
    };
    nordic = {
      package = pkgs.nordic;
      name = "Nordic";
    };
  };

in {
  config = {
    gtk = {
      enable = true;
      iconTheme = iconThemes.numix-circle;
      theme = themes.dracula;
    };
    xsession.pointerCursor = cursorThemes.numix;
  };
}
