{ config, lib, pkgs, ... }: {
  imports = [
    ../system/xserver.nix
    ../users
    ../users/myme.nix
  ];

  config = {
    # VM
    virtualisation.vmware.guest.enable = true;

    # Security
    security.sudo.wheelNeedsPassword = false;

    # User config
    myme.machine.role = "desktop";
    home-manager.users.myme =
      import ../home-manager {
        myme.alacritty.font_size = 15.0;

        # Development tools
        myme.dev = {
          docs.enable = true;
          haskell.enable = true;
          nodejs.enable = true;
          python.enable = true;
          shell.enable = true;
        };

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
      };
  };
}
