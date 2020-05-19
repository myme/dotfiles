{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.nixon;

  nixon = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "myme";
    repo = "nixon";
    rev = "6b90b61a39ade26e7074c63c6eea4c0ed4bd87c8";
    sha256 = "13ajx49h760p4fpy6gh4sivf6kdggcn8kjkxyrh50wcj52i63knn";
  }) {};

in {
  options.programs.nixon = {
    enable = mkEnableOption "nixon";

    package = mkOption {
      type = types.package;
      default = nixon;
      description = "Nixon package.";
    };

    source_dirs = mkOption {
      type = types.listOf types.str;
      default = [];
      defaultText = literalExample ''[ "~/src" "~/projects" ]'';
      description = "Directories to search for projects.";
    };

    exact_match = mkOption {
      type = types.bool;
      default = false;
      description = "Use exact matching (non-fuzzy) when selecting.";
    };

    use_direnv = mkOption {
      type = types.bool;
      default = false;
      description = "Evaluate .envrc files using direnv.";
    };

    use_nix = mkOption {
      type = types.bool;
      default = false;
      description = "Evaluate shell.nix & default.nix files using nix-shell.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."nixon.json".text = builtins.toJSON {
      inherit (cfg) source_dirs exact_match use_direnv use_nix;
    };

    programs.zsh.initExtra = (mkIf config.programs.zsh.enable ''
      # Nixon
      alias n=nixon

      # Nixon (p: project cd)
      unalias p
      p () {
          local project;
          project=$(nixon project -s "$@" | tail -1)
          if [ -z "$project" ]; then
              return
          fi
          cd "$project"
      }

      # Nixon (px: project execute)
      px () {
          nixon project . "$@"
      }

      # Nixon (x: execute)
      x () {
          nixon run . "$@"
      }

      source ${nixon}/share/zsh/site-functions/_nixon_widget
    '');
  };
}