{ lib, pkgs, ... }:
{
  config = {
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
