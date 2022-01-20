{ config, lib, pkgs, ... }: {
  imports = [
    ../system/xserver.nix
    ../users
    ../users/myme.nix
  ];

  config = {
    myme.machine.role = "desktop";
    home-manager.users.myme =
      import ../home-manager (attrs: attrs // {
        myme.wm = {
          enable = true;
          variant = "xmonad";
          conky = false;
          polybar.monitor = "Virtual-1";
        };
      });
  };
}
