{ config, lib, pkgs, ... }:

let
  cfg = config.myme.rofi;
  rofiTheme = pkgs.stdenv.mkDerivation {
    name = "myme-rofi-theme";
    srcs = ./dracula.rasi;
    dontUnpack = true;
    rofiFontFamily = cfg.font.family;
    rofiFontSize = cfg.font.size;
    installPhase = ''
      cp -av "$srcs" "$out"
    '';
    postFixup = ''
      substituteInPlace $out \
        --subst-var rofiFontFamily \
        --subst-var rofiFontSize
    '';
  };

in {
  options.myme.rofi = {
    enable = lib.mkEnableOption "Enable Rofi";
    font = {
      family = lib.mkOption {
        type = lib.types.str;
        default = "NotoSansMono Nerd Font";
        description = "Rofi font family";
      };
      size = lib.mkOption {
        type = lib.types.int;
        default = 13;
        description = "Rofi font size";
      };
    };
  };

  config = {
    programs.rofi = {
      enable = cfg.enable;
      theme = "${rofiTheme}";
    };
  };
}
