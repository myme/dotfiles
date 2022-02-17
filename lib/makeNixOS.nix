name: machineFile: { self, home-manager, nixpkgs, system, overlays }:
nixpkgs.lib.nixosSystem {
  inherit system;
  modules = [
    ../system
    ../users/root.nix
    home-manager.nixosModules.home-manager
    machineFile
    ({ lib, ... }: {
      # Hostname
      networking.hostName = name;

      # Let 'nixos-version --json' know about the Git revision
      # of this flake.
      system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;

      # Nix + nixpkgs
      nix.registry.nixpkgs.flake = nixpkgs;  # Pin flake nixpkgs
      nixpkgs.overlays = overlays;
    })
  ];
}
