{ config, lib, pkgs, ... }: {
  imports = [
    ../system/xserver.nix
    ../users
    ../users/myme.nix
  ];

  config = {
    # Network
    networking.networkmanager.enable = true;

    # Security
    security.sudo.wheelNeedsPassword = false;

    # User config
    myme.machine.role = "desktop";
    # myme.de.variant = "plasma";
    myme.de.variant = "wm";
    home-manager.users.myme =
      import ../home-manager {
        myme.wm = {
          enable = true;
          variant = "xmonad";
          # variant = "i3";
          conky = false;
          polybar.monitor = "Virtual-1";
        };
      };
  };
}
