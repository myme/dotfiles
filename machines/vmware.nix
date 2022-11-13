# VMWare
#
# Full graphical NixOS setup on VMWare.
#

{
  system = "x86_64-linux";
  config = { config, lib, pkgs, ... }: {
    imports = [
      ../system/xserver.nix
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
    };
  };
}
