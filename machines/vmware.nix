{ config, lib, pkgs, ... }: {
  imports = [
    ../system/xserver.nix
    ../users
    ../users/myme.nix
  ];

  config = {
    # Network + VM
    networking.interfaces.ens33.useDHCP = true;
    virtualisation.vmware.guest.enable = true;

    # Security
    security.sudo.wheelNeedsPassword = false;

    # User config
    myme.machine.role = "desktop";
    home-manager.users.myme =
      import ../home-manager (attrs: attrs // {
        myme.alacritty.font_size = 15.0;
        myme.wm = {
          enable = true;
          variant = "xmonad";
          conky = false;
          polybar = {
            font_size = 15;
            height = 50;
            monitor = "Virtual1";
          };
        };
      });
  };
}
