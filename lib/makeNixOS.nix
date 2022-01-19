name: { self, home-manager, nixpkgs, system }:
nixpkgs.lib.nixosSystem {
  inherit system;
  modules = [
    ../system
    ../users
    home-manager.nixosModules.home-manager
    {
      # Home manager
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;

      # Let 'nixos-version --json' know about the Git revision
      # of this flake.
      system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;

      # Pin flake nixpkgs
      nix.registry.nixpkgs.flake = nixpkgs;

      nixpkgs.overlays = [ self.overlay ];
    }
  ];
}
