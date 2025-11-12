{ config, lib, ... }:

let
  cfg = config.myme.ghostty;

in
{
  options.myme.ghostty = {
    enable = lib.mkEnableOption "Enable Ghostty terminal emulator";
  };

  config = lib.mkIf cfg.enable {
    programs.ghostty = {
      enable = true;
      settings = {
        background-opacity = 0.9;
        font-family = "NotoSansM Nerd Font";
        font-family-bold = "NotoSansM NF Bold";
        font-family-italic = "NotoSansM NF Italic";
        font-family-bold-italic = "NotoSansM NF Bold Italic";
        term = "xterm-256color";
        theme = "dracula";
        window-padding-x = 10;
        window-padding-y = 5;
      };
      themes.dracula = {
        background = "#282a36";
        cursor-color = "#f8f8f2";
        cursor-text = "#282a36";
        foreground = "#f8f8f2";
        palette = [
          "0=#21222c"
          "1=#ff5555"
          "2=#50fa7b"
          "3=#f1fa8c"
          "4=#bd93f9"
          "5=#ff79c6"
          "6=#8be9fd"
          "7=#f8f8f2"
          "8=#6272a4"
          "9=#ff6e6e"
          "10=#69ff94"
          "11=#ffffa5"
          "12=#d6acff"
          "13=#ff92df"
          "14=#a4ffff"
          "15=#ffffff"
        ];
        selection-foreground = "#f8f8f2";
        selection-background = "#44475a";
      };
    };
  };
}
