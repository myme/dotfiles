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
    in {
      overlay = final: prev: {
        myme = {
          inherit wallpapers;
          lib.allMachines = final.callPackage ./lib/allMachines.nix {  };
        };
      };

      nixosConfigurations = pkgs.myme.lib.allMachines {
        inherit self system nixpkgs home-manager;
      };

      devShell.${system} = pkgs.mkShell { };
    };
}
