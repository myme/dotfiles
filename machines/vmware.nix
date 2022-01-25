# VMWare
#
# Full graphical NixOS setup on VMWare.
#

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

    # Machine role
    myme.machine = {
      role = "desktop";
      highDPI = true;
    };

    # User config
    home-manager.users.myme = {
      imports = [
        ../home-manager
      ];

      config = {
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
          polybar.monitor = "Virtual1";
        };
      };
    };
  };
}
