{ config, pkgs, ... }:

let
  unstable = import <unstable> {};

  git-sync = pkgs.callPackage ./home/git-sync {};
  piddif = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "myme";
    repo = "piddif";
    rev = "8921849209abcb958d052eb3cfa3edd90030efb8";
    sha256 = "15ry0imjjh3by2fbb6rv0rww5jq1gpi7viy10vxvi2b55b4kigr4";
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
      settings = {
        left_meters = [ "LeftCPUs2" "Memory" "Swap" ];
        right_meters = [ "RightCPUs2" "Tasks" "LoadAverage" "Uptime" ];
      };
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
        format = "[$time]($style) ";
      };
    };
    zsh.enable = true;
  };
}
