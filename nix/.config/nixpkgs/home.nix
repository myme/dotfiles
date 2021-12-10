{ config, pkgs, ... }:

let
  git-sync = pkgs.callPackage ./home/git-sync {};

  piddif = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "myme";
    repo = "piddif";
    rev = "21aebd7eb4431f1965cd05aedf479c2a357f2976";
    sha256 = "G/pB11kyovmJIuJHlbN4/7ahclAg1kIsElxawfCRtms=";
  }) {};

in {
  imports = [
    ./home/alacritty
    ./home/bpytop.nix
    ./home/nixon.nix
    ./home/tmux.nix
    ./home/vim.nix
    ./home/zsh.nix
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

    bash.enable = true;
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
    rofi = {
      enable = true;
      theme = ./home/rofi/dracula.rasi;
    };
    starship = {
      enable = true;
      settings.time = {
        disabled = false;
        format = "[$time]($style) ";
      };
    };
  };
}
