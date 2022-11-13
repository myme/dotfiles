{
  system = "x86_64-linux";
  config = { config, lib, pkgs, ... }: {
    imports = [
      ../users/user.nix
    ];

    config = {
      # Security
      security.sudo.wheelNeedsPassword = false;

      # User config
      myme.machine.role = "server";
    };
  };
}
