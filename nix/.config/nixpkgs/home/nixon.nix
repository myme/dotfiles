{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.nixon;

  nixon = import (pkgs.fetchFromGitHub {
    owner = "myme";
    repo = "nixon";
    rev = "ff3637f1f9a8b71048b631818521fb8f11014b29";
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

    programs.bash.initExtra = (mkIf config.programs.bash.enable ''
      # Nixon
      alias n=nixon

      # Nixon (p: project cd)
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
          nixon project "$@"
      }

      source ${cfg.package}/share/nixon/nixon-widget.bash
    '');

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
          nixon project "$@"
      }

      source ${cfg.package}/share/zsh/site-functions/_nixon_widget
    '');
  };
}
