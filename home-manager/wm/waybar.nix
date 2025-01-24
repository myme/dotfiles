{ lib, ... }:

{
  options.myme.wm.waybar = {
    enable = lib.mkEnableOption "Enable waybar";
  };

  config = {
    programs.waybar.enable = true;
  };
}
