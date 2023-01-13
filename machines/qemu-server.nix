# QEmu
#
# Headless server installation of NixOS on QEmu.
#

{
  system = "x86_64-linux";
  config = { ... }: {
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
        profile = { };
      };
    };

    # Security
    security.sudo.wheelNeedsPassword = false;
  };
}
