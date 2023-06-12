{ config, lib, pkgs, specialArgs, ... }:

let
  defaultPrograms = config.myme.defaultPrograms;

in ({
  imports = [
    ./barrier.nix
    ./dev.nix
    ./emacs
    ./fonts.nix
    ./git.nix
    ./irc.nix
    ./nixon
    ./spotify.nix
    ./tmux.nix
    ./vim.nix
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
      nixfmt
      ripgrep
      tree
      unzip
      zip
      annodate
      myme.pkgs.git-sync
    ]);

    home.keyboard = {
      layout = "us";
      variant = "alt-intl-unicode";
    };

    # But of course!
    myme.emacs.enable = lib.mkDefault defaultPrograms;

    programs = {
      bat.enable = lib.mkDefault defaultPrograms;
      bash = {
        enable = true;
        historyControl = ["erasedups" "ignoredups" "ignorespace"];
      };
      direnv = {
        enable = lib.mkDefault defaultPrograms;
        nix-direnv.enable = true;
      };
      fish.enable = lib.mkDefault defaultPrograms;
      fzf = {
        enable = lib.mkDefault defaultPrograms;
        fileWidgetCommand = "fd --type f";
        fileWidgetOptions = ["--preview 'bat {}'"];
        changeDirWidgetCommand = "fd --type d";
        changeDirWidgetOptions = ["--preview 'tree -C {} | head -200'"];
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
        enable = true;
        settings.time = {
          disabled = false;
          format = "[$time]($style) ";
        };
      };
    };
  };
})
