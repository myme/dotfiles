{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    home-manager.url = "github:nix-community/home-manager/release-21.11";
    wallpapers = {
      url = "gitlab:myme/wallpapers";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, wallpapers }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      };
      name = "nixos";
    in {
      overlay = final: prev: { myme = { inherit wallpapers; }; };

      nixosConfigurations.${name} = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./system
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

            nixpkgs.overlays = [ self.overlay ];
          }
        ];
      };

      devShell.${system} = pkgs.mkShell { };
    };
}
