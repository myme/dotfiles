{ config, lib, pkgs, nixos-config, ... }:

let
  defaultPrograms = config.myme.defaultPrograms;

in ({
  imports = [
    ./barrier.nix
    ./btop.nix
    ./dev.nix
    ./emacs
    ./fonts.nix
    ./git.nix
    ./irc.nix
    ./nixon
    ./spotify.nix
    ./tmux.nix
    ./vim
    ./wm
    ./wsl.nix
  ];

  options.myme.defaultPrograms = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = "Include a default set of programs and services.";
  };

  config = {
    home.packages = lib.mkIf defaultPrograms (with pkgs; [
      dua
      fd
      jq
      lsof
      nix-tree
      nixfmt-rfc-style
      ripgrep
      tree
      unzip
      xh
      yq
      zip
      annodate
      myme.pkgs.git-sync
      myme.pkgs.hm
    ]);

    home.keyboard = {
      layout = "us";
      variant = "alt-intl-unicode";
    };

    # But of course!
    myme.emacs.enable = lib.mkDefault defaultPrograms;
    myme.vim.enable = lib.mkDefault defaultPrograms;

    programs = {
      bat.enable = lib.mkDefault defaultPrograms;
      bash = {
        enable = true;
        historyControl = ["erasedups" "ignoredups" "ignorespace"];
        initExtra = lib.mkIf nixos-config.virtualisation.docker.enable ''
          # Docker
          alias dc="docker compose"
        '';
      };
      direnv = {
        enable = lib.mkDefault defaultPrograms;
        enableNushellIntegration = true;
        nix-direnv.enable = true;
        stdlib = builtins.readFile ./direnvrc;
      };
      fish.enable = lib.mkDefault defaultPrograms;
      fzf = {
        enable = lib.mkDefault defaultPrograms;
        fileWidgetCommand = "fd --type f";
        fileWidgetOptions = ["--preview 'bat {}'"];
        changeDirWidgetCommand = "fd --type d";
        changeDirWidgetOptions = ["--preview 'tree -C {} | head -200'"];
      };
      helix = {
        enable = lib.mkDefault defaultPrograms;
        settings = {
          theme = "dracula";
          editor = {
            line-number = "relative";
          };
        };
      };
      htop = {
        enable = lib.mkDefault defaultPrograms;
        settings = {
          left_meters = [ "LeftCPUs2" "Memory" "Swap" ];
          right_meters = [ "RightCPUs2" "Tasks" "LoadAverage" "Uptime" ];
        };
      };
      nixon = {
        enable = lib.mkDefault defaultPrograms;
        source_dirs = [
          "~/code/*"
          "~/nixos"
          "~/notes"
          "~/src"
        ];
        exact_match = true;
        ignore_case = true;
        use_direnv = true;
        use_nix = true;
      };
      nushell.enable = lib.mkDefault defaultPrograms;
      starship = {
        enable = lib.mkDefault defaultPrograms;
        settings.time = {
          disabled = false;
          format = "[$time]($style) ";
        };
      };
    };
  };
})
