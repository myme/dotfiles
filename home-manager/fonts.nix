{ config, lib, pkgs, ... }:

let
  cfg = config.myme.fonts;

in {
  options.myme.fonts = {
    enable = lib.mkEnableOption "WM Fonts";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # Font browser
      font-manager

      # Fonts
      dejavu_fonts
      font-awesome_4
      material-icons
      powerline-fonts

      # Nerd fonts
      nerd-fonts.dejavu-sans-mono
      nerd-fonts.noto
      nerd-fonts.sauce-code-pro
    ];

    # For great fonts
    fonts.fontconfig.enable = lib.mkForce true;
  };
}
