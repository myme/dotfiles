{
  description = "myme's NixOS configuration with flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
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
    deploy-rs.url = "github:serokell/deploy-rs";
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

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    let
      overlays = [
        inputs.agenix.overlays.default
        inputs.deploy-rs.overlay
        inputs.i3ws.overlay
        inputs.annodate.overlay
        inputs.nixon.overlay
        self.overlays.default
      ];
      lib = nixpkgs.lib.extend (final: prev:
        import ./lib {
          inherit inputs overlays;
          lib = final;
        });
    in {
      # Personal overlays
      overlays.default = import ./overlay.nix {
        inherit lib;
        inherit (inputs) doomemacs home-manager wallpapers;
      };

      # NixOS machines
      nixosConfigurations = lib.myme.allProfiles lib.myme.makeNixOS;

      # Deploy nodes
      deploy.nodes =
        lib.myme.allProfilesIf (_: host: host ? deploy) (lib.myme.deployConf {
          inherit (inputs) deploy-rs;
          inherit (self) nixosConfigurations;
        });

      # Deploy checks
      checks = builtins.mapAttrs
        (system: deployLib: deployLib.deployChecks self.deploy)
        inputs.deploy-rs.lib;

      # Non-NixOS machines (Fedora, WSL, ++)
      homeConfigurations = lib.myme.nixos2hm {
        inherit (self) nixosConfigurations;
      };

      # Installation mediums
      sdImages = builtins.mapAttrs
        (name: config: config.config.system.build.sdImage)
        self.nixosConfigurations;
    } // flake-utils.lib.eachSystem ["aarch64-linux" "x86_64-linux"] (system:
      let pkgs = import nixpkgs { inherit system overlays; };
      in {
        # Apps for `nix run .#<app>`
        apps = {
          agenix = {
            type = "app";
            program = "${pkgs.agenix}/bin/agenix";
          };
          deploy = {
            type = "app";
            program = "${pkgs.deploy-rs.deploy-rs}/bin/deploy";
          };
        };

        # All packages under pkgs.myme.pkgs from the overlay
        packages = pkgs.myme.pkgs;

        devShells = {
          # Default dev shell (used by direnv)
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              agenix
            ];
          };

          # Deployment to other nodes
          deploy = pkgs.mkShell {
            buildInputs = with pkgs; [
              deploy-rs.deploy-rs
            ];
          };

          # For hacking on XMonad
          xmonad = pkgs.mkShell {
            buildInputs = with pkgs;
              [ (ghc.withPackages (ps: with ps; [ xmonad xmonad-contrib ])) ];
          };
        };
      });
}
