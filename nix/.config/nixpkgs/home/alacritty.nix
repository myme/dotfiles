{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.myme.alacritty;

in {
  options.myme.alacritty = {
    background_opacity = mkOption {
      type = types.float;
      default = 0.95;
      description = "Alacritty background opacity";
    };
    font_size = mkOption {
      type = types.float;
      default = 12.0;
      description = "Alacritty font size";
    };
  };

  config = {
    programs.alacritty = {
      enable = true;
      settings = {
        background_opacity = cfg.background_opacity;
        env = {
          TERM = "xterm-256color";
        };
        font = {
          normal.family = "DejaVu Sans Mono for Powerline";
          size = cfg.font_size;
        };
        colors = {
          # Default colors
          primary = {
            background = "0x282c34";
            foreground = "0xabb2bf";
          };

          # Selection
          selection = {
            text = "#cc3e6c";
            background = "#ffb4cc";
          };

          # Normal colors
          normal = {
            black   = "0x131613";
            red     = "0xe06c75";
            green   = "0x98c379";
            yellow  = "0xd19a66";
            blue    = "0x61afef";
            magenta = "0xc678dd";
            cyan    = "0x56b6c2";
            white   = "0xabb2bf";
          };

          # Bright colors
          bright = {
            black   = "0x5c6370";
            red     = "0xe06c75";
            green   = "0x98c379";
            yellow  = "0xd19a66";
            blue    = "0x61afef";
            magenta = "0xc678dd";
            cyan    = "0x56b6c2";
            white   = "0xffffff";
          };
        };
      };
    };
  };
}
