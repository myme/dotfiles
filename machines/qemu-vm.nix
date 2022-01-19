{ config, lib, pkgs, ... }: {
  config = {
    myme.machine = { role = "desktop"; };
    home-manager.users.myme = {
      myme.wm = {
        enable = true;
        variant = "xmonad";
        conky = false;
        polybar.monitor = "Virtual-1";
      };
    };
  };
}
