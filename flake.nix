{
  description = "myme's NixOS configuration with flakes";

  inputs = {
    # NixOS + Nixpkgs
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Disko
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Home manager
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-stable = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };

    # Tools + Utils
    deploy-rs.url = "github:serokell/deploy-rs";
    flake-utils.url = "github:numtide/flake-utils";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    doomemacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
    i3ws = {
      url = "github:myme/i3ws";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    annodate.url = "github:myme/annodate";
    nixon.url = "github:myme/nixon";
    piddif.url = "github:myme/piddif";
    wallpapers = {
      url = "gitlab:myme/wallpapers";
      flake = false;
    };

    # Themes
    alacritty-catppuccin = {
      url = "github:catppuccin/alacritty";
      flake = false;
    };
    alacritty-dracula = {
      url = "github:dracula/alacritty";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }@inputs:
    let
      overlays = [
        inputs.agenix.overlays.default
        inputs.deploy-rs.overlay
        inputs.i3ws.overlay
        inputs.annodate.overlay
        inputs.nixon.overlay
        inputs.piddif.overlay
        self.overlays.default
      ];
      lib = nixpkgs.lib.extend (
        final: _prev:
        import ./lib {
          inherit inputs overlays;
          lib = final;
        }
      );
    in
    {
      # Personal overlays
      overlays.default = import ./overlay.nix {
        inherit lib;
        inherit (inputs)
          doomemacs
          nixpkgs
          wallpapers
          ;
      };

      # NixOS machines
      nixosConfigurations = lib.myme.allProfiles lib.myme.makeNixOS;

      # Deploy nodes
      deploy.nodes = lib.myme.allProfilesIf (_: host: host ? deploy) (
        lib.myme.deployConf {
          inherit (inputs) deploy-rs;
          inherit (self) nixosConfigurations;
        }
      );

      # Non-NixOS machines (Fedora, WSL, ++)
      homeConfigurations = lib.myme.nixos2hm { inherit (self) nixosConfigurations; };

      # Installation mediums
      sdImages = builtins.mapAttrs (
        _name: config: config.config.system.build.sdImage
      ) self.nixosConfigurations;
    }
    //
      flake-utils.lib.eachSystem
        [
          "aarch64-linux"
          "x86_64-linux"
          "aarch64-darwin"
        ]
        (
          system:
          let
            pkgs = import nixpkgs { inherit system overlays; };
            isLinux = lib.hasSuffix "-linux" system;
            treefmtEval = inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
            pre-commit-check = inputs.git-hooks.lib.${system}.run {
              src = ./.;
              default_stages = [ "pre-push" ];
              hooks = {
                treefmt = {
                  enable = true;
                  package = treefmtEval.config.build.wrapper;
                };
                statix.enable = true;
                deadnix.enable = true;
                gitleaks = {
                  enable = true;
                  name = "gitleaks";
                  description = "Detect hardcoded secrets";
                  entry = "${pkgs.gitleaks}/bin/gitleaks detect --no-banner --redact --no-git --source=.";
                  language = "system";
                  pass_filenames = false;
                };
              };
            };
          in
          {
            # `nix fmt`
            formatter = treefmtEval.config.build.wrapper;

            # `nix flake check`
            checks = {
              pre-commit = pre-commit-check;
            }
            // lib.optionalAttrs (inputs.deploy-rs.lib ? ${system}) (
              # Only check deploy nodes that target the current system —
              # cross-arch builds would require qemu emulation in CI.
              inputs.deploy-rs.lib.${system}.deployChecks {
                nodes = lib.filterAttrs (
                  name: _: self.nixosConfigurations.${name}.pkgs.stdenv.hostPlatform.system == system
                ) self.deploy.nodes;
              }
            );

            # Apps for `nix run .#<app>` (Linux only)
            apps = lib.optionalAttrs isLinux {
              agenix = {
                type = "app";
                program = "${pkgs.agenix}/bin/agenix";
              };
              deploy = {
                type = "app";
                program = "${pkgs.deploy-rs.deploy-rs}/bin/deploy";
              };
            };

            # All packages under pkgs.myme.pkgs from the overlay (Linux only)
            packages = lib.optionalAttrs isLinux pkgs.myme.pkgs;

            devShells = {
              # Default dev shell (used by direnv) — installs the pre-push hook
              default = pkgs.mkShell {
                inherit (pre-commit-check) shellHook;
                buildInputs =
                  pre-commit-check.enabledPackages
                  ++ lib.optionals isLinux (
                    with pkgs;
                    [
                      agenix
                      disko
                      myme.pkgs.nixos-bootstrap
                    ]
                  );
              };
            }
            // lib.optionalAttrs isLinux {
              # Deployment to other nodes
              deploy = pkgs.mkShell { buildInputs = with pkgs; [ deploy-rs.deploy-rs ]; };

              # For hacking on XMonad
              xmonad = pkgs.mkShell {
                buildInputs = with pkgs; [
                  (ghc.withPackages (
                    ps: with ps; [
                      xmonad
                      xmonad-contrib
                    ]
                  ))
                ];
              };
            };
          }
        );
}
