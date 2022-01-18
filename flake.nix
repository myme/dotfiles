{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    home-manager.url = "github:nix-community/home-manager/release-21.11";
  };

  outputs = { self, nixpkgs, home-manager }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./machine
          ./users
          home-manager.nixosModules.home-manager
          {
            # Home manager
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            # Let 'nixos-version --json' know about the Git revision
            # of this flake.
            system.configurationRevision =
              nixpkgs.lib.mkIf (self ? rev) self.rev;

            # Pin flake nixpkgs
            nix.registry.nixpkgs.flake = nixpkgs;
          }
        ];
      };

      devShell.${system} = pkgs.mkShell { };
    };
}
