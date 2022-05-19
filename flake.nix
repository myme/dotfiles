{
  description = "myme's NixOS configuration with flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-wsl.url = "github:nix-community/NixOS-WSL";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    doomemacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
    i3ws.url = "github:myme/i3ws";
    annodate.url = "github:myme/annodate";
    nixon.url = "github:myme/nixon";
    wallpapers = {
      url = "gitlab:myme/wallpapers";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      overlays = [
        inputs.agenix.overlay
        inputs.i3ws.overlay
        inputs.annodate.overlay
        inputs.nixon.overlay
        self.overlay
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
      };
    in rec {
      overlay = import ./overlay.nix {
        inherit home-manager;
        inherit (inputs) doomemacs wallpapers;
      };

      # All packages under pkgs.myme.apps from the overlay
      packages.${system} = pkgs.myme.apps;

      # NixOS machines
      nixosConfigurations = pkgs.myme.lib.allProfiles ./machines (name: file:
        pkgs.myme.lib.makeNixOS name file {
          inherit inputs system overlays;
        });

      # Non-NixOS machines (Fedora, WSL, ++)
      homeConfigurations = pkgs.myme.lib.nixos2hm {
        inherit overlays system nixosConfigurations;
      };

      devShells.${system} = {
        # Default dev shell (used by direnv)
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            agenix
          ];
        };

        # For hacking on XMonad
        xmonad = pkgs.mkShell {
          buildInputs = with pkgs; [
            (ghc.withPackages (ps: with ps; [
              xmonad
              xmonad-contrib
            ]))
          ];
        };
      };
    };
}
