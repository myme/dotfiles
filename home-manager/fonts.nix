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
      (nerdfonts.override { fonts = [ "DejaVuSansMono" "Noto" "SourceCodePro" ]; })
    ];

    # For great fonts
    fonts.fontconfig.enable = lib.mkForce true;
  };
}
