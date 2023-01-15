# QEmu
#
# Headless server installation of NixOS on QEmu.
#

{
  system = "x86_64-linux";
  config = {
    myme.machine = {
      role = "server";
      flavor = "nixos";
      user = {
        name = "nixos";
        config = {
          isNormalUser = true;
          initialPassword = "nixos";
          extraGroups = [ "wheel" ];
        };
        profile = {
          imports = [
            ../home-manager
          ];
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
