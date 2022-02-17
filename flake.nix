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
    nixon.url = "github:myme/nixon";
    wallpapers = {
      url = "gitlab:myme/wallpapers";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@args:
    let
      system = "x86_64-linux";
      overlays = [
        args.i3ws.overlay
        args.nixon.overlay
        self.overlay
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
      };
    in rec {
      overlay = import ./overlay.nix {
        inherit home-manager;
        inherit (args) doom-emacs wallpapers;
      };

      # All packages under pkgs.myme.apps from the overlay
      packages.${system} = pkgs.myme.apps;

      # NixOS machines
      nixosConfigurations = pkgs.myme.lib.allProfiles ./machines (name: file:
        pkgs.myme.lib.makeNixOS name file {
          inherit self system nixpkgs home-manager overlays;
        });

      # Non-NixOS machines (Fedora, WSL, ++)
      homeConfigurations = pkgs.myme.lib.nixos2hm {
        inherit overlays system nixosConfigurations;
      };

      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          # For hacking on XMonad
          (pkgs.ghc.withPackages (ps: with ps; [
            xmonad
            xmonad-contrib
          ]))
        ];
      };
    };
}
