name: machineFile: { self, home-manager, nixpkgs, system }:
nixpkgs.lib.nixosSystem {
  inherit system;
  modules = [
    ../system
    ../machines/default.nix
    machineFile
    ../users
    home-manager.nixosModules.home-manager
    {
      # Hostname
      networking.hostName = name;

      # Home manager
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;

      # Let 'nixos-version --json' know about the Git revision
      # of this flake.
      system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;

      # Pin flake nixpkgs
      nix.registry.nixpkgs.flake = nixpkgs;

      nixpkgs.overlays = self.overlays;
    }
  ];
}
