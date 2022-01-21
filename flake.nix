{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    doom-emacs = {
      url = "github:hlissner/doom-emacs";
      flake = false;
    };
    i3ws.url = "github:myme/i3ws";
    wallpapers = {
      url = "gitlab:myme/wallpapers";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@args:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = self.overlays;
      };
    in {
      overlays = [
        args.i3ws.overlay
        (final: prev: {
          myme = {
            inherit (args) doom-emacs wallpapers;
            lib = final.callPackage ./lib {  };
          };
        })
      ];

      nixosConfigurations = pkgs.myme.lib.allMachines {
        inherit self system nixpkgs home-manager;
      };

      # Non-NixOS (Fedora, WSL, ++)
      homeConfigurations = {
        wsl = import ./machines/wsl.nix {
          inherit home-manager system;
          overlays = self.overlays;
        };
      };

      wsl = self.homeConfigurations.wsl.activationPackage;

      devShell.${system} = pkgs.mkShell { };
    };
}
