# QEmu
#
# Full graphical NixOS setup on QEmu.
#

{
  system = "x86_64-linux";
  config = { config, lib, pkgs, ... }: {
    myme.machine = {
      role = "desktop";
      de.variant = "hyprland";
      user = {
        name = "nixos";

        # This maps to the `users.users.nixos` NixOS config
        config = {
          isNormalUser = true;
          initialPassword = "nixos";
          extraGroups = [ "wheel" ];
          openssh.authorizedKeys.keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHMffZhwksuToW2ceS2I7os/X/QTRNUx4wxHoVRVaALR myme@heap"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIlMUotM7KE9qbVmLQbrp9+gvw8bwtrSU2aEYIG59saC myme@trie"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH+9tnNlMesGrK/lDvycgzyS4pPrsGqcGQP6yLCsr/LN myme@Tuple"
          ];
        };

        # This maps to the `home-manager.users.nixos` NixOS (HM module) config
        profile = {
          imports = [
            ../home-manager
          ];

          config = {
            myme.wm = {
              enable = true;
              variant = "hyprland";
              # variant = "xmonad";
              # conky.enable = false;
              # polybar.monitor = "Virtual-1";
            };
          };
        };
      };
    };

    # Security
    security.sudo.wheelNeedsPassword = false;
  };
}
