{ pkgs, ... }:
{
  config = {
    home.packages = with pkgs; [
      dejavu_fonts
      font-awesome_4
      material-icons
      powerline-fonts
    ];

    # For great fonts
    fonts.fontconfig.enable = true;
  };
}
