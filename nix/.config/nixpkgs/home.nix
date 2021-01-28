{ config, pkgs, ... }:

let
  git-sync = pkgs.callPackage ./home/git-sync {};
  piddif = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "myme";
    repo = "piddif";
    rev = "7df2ecadec4441b97200eb02b41a62a9bdf54633";
    sha256 = "128y7mkyp2qqj968i34yfan8bgp8364x5h7xpzb2ly3zaw41d5i1";
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
  ] ++ [
    git-sync
    piddif
  ];

  # Keyboard
  home.keyboard = {
    layout = "us";
    variant = "alt-intl-unicode";
  };

  myme.alacritty = {
    background_opacity = 0.95;
    font_size = 6.0;
    # theme = "nord";
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
    direnv = import ./home/direnv.nix;
    emacs.enable = true;
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
