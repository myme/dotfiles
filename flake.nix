{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    home-manager.url = "github:nix-community/home-manager/release-21.11";
    i3ws.url = "github:myme/i3ws";
    wallpapers = {
      url = "gitlab:myme/wallpapers";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, i3ws, wallpapers }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = self.overlays;
      };
    in {
      overlays = [
        i3ws.overlay
        (final: prev: {
          myme = {
            inherit wallpapers;
            lib = final.callPackage ./lib {  };
          };
        })
      ];

      nixosConfigurations = pkgs.myme.lib.allMachines {
        inherit self system nixpkgs home-manager;
      };

      devShell.${system} = pkgs.mkShell { };
    };
}
