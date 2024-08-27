{ config, lib, pkgs, ... }:
let
  cfg = config.myme.wm.hyprland;
in
{
  options.myme.wm.hyprland = {
    enable = lib.mkEnableOption "Hyprland - Tiling compositor with the looks";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.wofi
    ];

    programs.hyprlock.enable = true;

    wayland.windowManager.hyprland = {
      enable = true;
      xwayland.enable = true;
      systemd.enable = true;
    };
  };
}
