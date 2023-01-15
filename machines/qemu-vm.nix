# QEmu
#
# Full graphical NixOS setup on QEmu.
#

{
  system = "x86_64-linux";
  config = { config, lib, pkgs, ... }: {
    myme.machine = {
      role = "desktop";
      flavor = "nixos";
      user = {
        name = "nixos";

        # This maps to the `users.users.nixos` NixOS config
        config = {
          isNormalUser = true;
          initialPassword = "nixos";
          extraGroups = [ "wheel" ];
        };

        # This maps to the `home-manager.users.nixos` NixOS (HM module) config
        profile = {
          imports = [
            ../home-manager
          ];

          config = {
            myme.wm = {
              enable = true;
              variant = "xmonad";
              conky = false;
              polybar.monitor = "Virtual-1";
            };
          };
        };
      };
    };

    # Security
    security.sudo.wheelNeedsPassword = false;

    # Mock FS
    fileSystems."/" = {
      device = "/dev/null";
      fsType = "ext4";
    };
  };
}
