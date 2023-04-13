{ config, lib, pkgs, ... }:

with lib;

let cfg = config.programs.nixon;

in {
  options.programs.nixon = {
    enable = mkEnableOption "nixon";

    package = mkOption {
      type = types.package;
      default = pkgs.nixon;
      description = "Nixon package.";
    };

    source_dirs = mkOption {
      type = types.listOf types.str;
      default = [ ];
      defaultText = literalExample ''[ "~/src" "~/projects" ]'';
      description = "Directories to search for projects.";
    };

    ignore_case = mkOption {
      type = types.bool;
      default = false;
      description = "Ignore case sensitivity when searching.";
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

    configExtra = mkOption {
      type = types.str;
      default = lib.readFile ./nixon.md;
      description = "Configurations added to nixon.md";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."nixon.md".text = ''
      # Nixon

      ## Config {.config}

      ```json
      ${builtins.toJSON {
        inherit (cfg) exact_match ignore_case use_direnv use_nix;
        project_dirs = cfg.source_dirs;
        project_types = [
          {
            name = "cabal";
            test = [ "cabal.project" ];
            desc = "Cabal new-style project";
          }
          {
            name = "npm";
            test = [ "package.json" ];
            desc = "NPM project";
          }
          {
            name = "yarn";
            test = [ "yarn.lock" ];
            desc = "Yarn project";
          }
          {
            name = "nix";
            test = [ "flake.nix" "default.nix" "shell.nix" ];
            desc = "Nix project";
          }
          {
            name = "direnv";
            test = [ ".envrc" ];
            desc = "Direnv project";
          }
          {
            name = "git";
            test = [ ".git" ];
            desc = "Git repository";
          }
          {
            name = "hg";
            test = [ ".hg" ];
            desc = "Mercurial project";
          }
          {
            name = "project";
            desc = "Generic project";
          }
        ];
      }}
      ```

      ${cfg.configExtra}
    '';

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
