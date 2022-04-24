{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-wsl.url = "github:nix-community/NixOS-WSL";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    doom-emacs = {
      url = "github:hlissner/doom-emacs";
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
        inherit (inputs) doom-emacs wallpapers;
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

      devShell.${system} = pkgs.mkShell {
        buildInputs = with pkgs; [
          # For hacking on XMonad
          (ghc.withPackages (ps: with ps; [
            xmonad
            xmonad-contrib
          ]))
        ];
      };
    };
}
