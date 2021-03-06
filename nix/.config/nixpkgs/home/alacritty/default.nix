{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.myme.alacritty;
  terminal = pkgs.writeShellScriptBin "x-terminal-emulator" ''${pkgs.alacritty}/bin/alacritty "$@"'';
  theme = import (./. + "/${cfg.theme}-theme.nix");

in {
  options.myme.alacritty = {
    enable = mkEnableOption "Enable Alacritty";
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
    theme = mkOption {
      type = types.str;
      default = "one";
      description = "Alacritty theme";
    };
  };

  config = {
    home.packages = [
      terminal
    ];

    programs.alacritty = {
      enable = cfg.enable;
      settings = {
        background_opacity = cfg.background_opacity;
        env = {
          TERM = "xterm-256color";
        };
        font = {
          normal.family = "DejaVu Sans Mono for Powerline";
          size = cfg.font_size;
        };
        colors = theme;
      };
    };
  };
}
