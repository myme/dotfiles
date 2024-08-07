{
  system = "x86_64-linux";
  config = { lib, ... }: {
    # Install profiles into ~/.nix-profile
    home-manager.useUserPackages = lib.mkForce false;

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
