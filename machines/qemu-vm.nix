# QEmu
#
# Full graphical NixOS setup on QEmu.
#

{ config, lib, pkgs, ... }: {
  imports = [
    ../system/xserver.nix
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
  };
}
