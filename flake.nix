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

      # NixOS machines
      nixosConfigurations = pkgs.myme.lib.allMachines (name: file:
        pkgs.myme.lib.makeNixOS name file {
          inherit self system nixpkgs home-manager;
        });

      # Non-NixOS machines (Fedora, WSL, ++)
      homeConfigurations = pkgs.myme.lib.allMachines (_: file:
        home-manager.lib.homeManagerConfiguration (import file {
          inherit system;
          overlays = self.overlays;
        } // {
          extraSpecialArgs = {
            # Shim NixOS config on non-NixOS systems
            nixosConfig.myme.machine = {
              role = "desktop";
              highDPI = false;
            };
          };
        }));

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
