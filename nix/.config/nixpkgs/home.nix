{ config, pkgs, ... }:

let
  unstable = import <unstable> {};

  git-sync = pkgs.callPackage ./home/git-sync {};
  piddif = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "myme";
    repo = "piddif";
    rev = "97d9cef8918c81fddd7db052f88d2295e832089d";
    sha256 = "05y7hh636nr6niljg368hfwv5kc0939pmx53x78cr4hj8qsn6q85";
  }) {};

in {
  imports = [
    ./home/alacritty
    ./home/bpytop.nix
    ./home/nixon.nix
    ./home/prezto.nix
    ./home/tmux.nix
    ./home/vim.nix
  ];

  home.packages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [
      en
      en-computers
      nb
    ]))
    fd
    jq
    mosh
    ncdu
    pcmanfm
    ripgrep
    tree
    unzip
    zip
  ] ++ (with unstable; [
    rofimoji
  ]) ++ [
    git-sync
    piddif
  ];

  # Keyboard
  home.keyboard = {
    layout = "us";
    variant = "alt-intl-unicode";
  };

  myme.alacritty = {
    enable = false;
    background_opacity = 0.95;
    font_size = 6.0;
    theme = "dracula";
  };

  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    nixon = {
      enable = true;
      exact_match = true;
      source_dirs = [
        "~/dotfiles"
        "~/src"
      ];
      use_direnv = true;
      use_nix = true;
    };

    bat.enable = true;
    direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
    };
    emacs = {
      enable = true;
      extraPackages = epkgs: with epkgs; [
        vterm
      ];
    };
    firefox.enable = true;
    fzf.enable = true;
    git = import ./home/git.nix;
    htop = {
      enable = true;
      meters = {
        left = [ "LeftCPUs2" "Memory" "Swap" ];
        right = [ "RightCPUs2" "Tasks" "LoadAverage" "Uptime" ];
      };
    };
    info.enable = true;
    man = {
      enable = true;
      generateCaches = true;
    };
    prezto.enable = true;
    rofi = {
      enable = true;
      theme = "Arc-Dark";
    };
    starship = {
      enable = true;
      settings.time = {
        disabled = false;
        format = "%T";
      };
    };
    zsh.enable = true;
  };
}
