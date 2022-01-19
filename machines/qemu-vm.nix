{ config, lib, pkgs, ... }:
{
  config = {
    myme.machine = {
      role = "desktop";
    };
    myme.wm = {
      enable = true;
      variant = "xmonad";
      conky = false;
      polybar.monitor = "Virtual-1";
    };
  };
}
