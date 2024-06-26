{ config, lib, pkgs, flake-inputs, ... }:

with lib;

let
  cfg = config.myme.alacritty;
  terminal = pkgs.writeShellScriptBin "x-terminal-emulator" ''${pkgs.alacritty}/bin/alacritty "$@"'';
  readTheme = input: theme: builtins.fromTOML (builtins.readFile "${input}/${theme}.toml");
  catppuccin = flake-inputs.alacritty-catppuccin;
  theme = {
    catppuccin-frappe = readTheme catppuccin "catppuccin-frappe";
    catppuccin-latte = readTheme catppuccin "catppuccin-latte";
    catppuccin-macchiato = readTheme catppuccin "catppuccin-macchiato";
    catppuccin-mocha = readTheme catppuccin "catppuccin-mocha";
    dracula = readTheme flake-inputs.alacritty-dracula "dracula";
    nord.colors = import ./nord-theme.nix;
  }.${cfg.theme};

in {
  options.myme.alacritty = {
    enable = mkEnableOption "Enable Alacritty";
    background_opacity = mkOption {
      type = types.float;
      default = 0.95;
      description = "Alacritty background opacity";
    };
    font_size = mkOption {
      type = types.int;
      default = 12;
      description = "Alacritty font size";
    };
    theme = mkOption {
      type = types.str;
      default = "one";
      description = "Alacritty theme";
    };
  };

  config = {
    home.packages = lib.mkIf cfg.enable [
      terminal
    ];

    programs.alacritty = {
      enable = cfg.enable;
      settings = theme // {
        env = {
          TERM = "xterm-256color";
        };
        font = {
          # normal.family = "DejaVu Sans Mono for Powerline";
          normal.family = "Noto Mono for Powerline";
          size = cfg.font_size;
        };
        window = {
          opacity = cfg.background_opacity;
          padding = { x = 10; y = 5; };
        };
      };
    };
  };
}
