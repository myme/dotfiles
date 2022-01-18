{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    home-manager.url = "github:nix-community/home-manager/release-21.11";
  };

  outputs = { self, nixpkgs, home-manager }: {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./hardware.nix
        ./config.nix
        ./users.nix
        home-manager.nixosModules.home-manager
        {
          # Home manager
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.myme = import ./home.nix;

          # Let 'nixos-version --json' know about the Git revision
          # of this flake.
          system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;

          # Pin flake nixpkgs
          nix.registry.nixpkgs.flake = nixpkgs;
        }
      ];
    };
  };
}
