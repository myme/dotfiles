{
  description = "myme's NixOS configuration with flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-wsl.url = "github:nix-community/NixOS-WSL";
    flake-utils.url = "github:numtide/flake-utils";
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

  outputs = { self, nixpkgs, flake-utils, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      overlays = [
        inputs.agenix.overlay
        inputs.i3ws.overlay
        inputs.annodate.overlay
        inputs.nixon.overlay
        self.overlay
      ];
      lib = nixpkgs.lib.extend (final: prev:
        import ./lib {
          inherit home-manager;
          lib = final;
        });
    in {
      overlay = import ./overlay.nix {
        inherit lib home-manager;
        inherit (inputs) doomemacs wallpapers;
      };

      # NixOS machines
      nixosConfigurations = lib.myme.allProfiles ./machines (name: file:
        lib.myme.makeNixOS name file { inherit inputs system overlays; });

      # Non-NixOS machines (Fedora, WSL, ++)
      homeConfigurations = lib.myme.nixos2hm {
        inherit (self) nixosConfigurations;
        inherit overlays system;
      };
    } // flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system overlays; };
      in {
        # All packages under pkgs.myme.apps from the overlay
        packages = pkgs.myme.apps;

        devShells = {
          # Default dev shell (used by direnv)
          default = pkgs.mkShell { buildInputs = with pkgs; [ agenix ]; };

          # For hacking on XMonad
          xmonad = pkgs.mkShell {
            buildInputs = with pkgs;
              [ (ghc.withPackages (ps: with ps; [ xmonad xmonad-contrib ])) ];
          };
        };
      });
}
