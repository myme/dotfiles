{ lib, pkgs, ... }:
{
  config = {
    home.packages = with pkgs; [
      # font-manager
      font-manager

      # fonts
      dejavu_fonts
      font-awesome_4
      material-icons
      powerline-fonts

      (nerdfonts.override { fonts = [ "DejaVuSansMono" "Noto" "SourceCodePro" ]; })
    ];

    # For great fonts
    fonts.fontconfig.enable = lib.mkForce true;
  };
}
