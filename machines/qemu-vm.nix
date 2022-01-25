# QEmu
#
# Full graphical NixOS setup on QEmu.
#

{ config, lib, pkgs, ... }: {
  imports = [
    ../system/xserver.nix
    ../users
    ../users/user.nix
  ];

  config = {
    # Network
    networking.networkmanager.enable = true;

    # Security
    security.sudo.wheelNeedsPassword = false;

    # Machine role + Desktop Environment
    myme.machine.role = "desktop";
    # myme.de.variant = "plasma";
    myme.de.variant = "wm";

    # User config
    home-manager.users.user = {
      imports = [
        ../home-manager
      ];

      config = {
        myme.alacritty.font_size = 10;
        myme.wm = {
          enable = true;
          variant = "xmonad";
          # variant = "i3";
          conky = false;
          polybar.monitor = "Virtual-1";
        };
      };
    };
  };
}
