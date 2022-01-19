{ config, lib, pkgs, ... }:
{
  config = {
    myme.wm = {
      enable = true;
      variant = "xmonad";
      polybar.monitor = "Virtual-1";
    };
  };
}
