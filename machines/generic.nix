{
  system = "x86_64-linux";
  config =
    { lib, ... }:
    {
      # Install profiles into ~/.nix-profile
      home-manager.useUserPackages = lib.mkForce false;

      # Stub — this config is only consumed by nixos2hm for non-NixOS Linux,
      # never booted, but `nix flake check` still requires the assertion.
      fileSystems."/" = {
        device = "none";
        fsType = "tmpfs";
      };

      myme.machine = {
        role = "server";
        user = {
          name = "martin";
          config = {
            isNormalUser = true;
          };
          profile = {
            imports = [ ../home-manager ];

            myme.emacs.enable = false;
            programs.nushell.enable = false;
          };
        };
      };
    };
}
