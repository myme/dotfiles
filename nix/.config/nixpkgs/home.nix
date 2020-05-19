{ config, pkgs, ... }:
{
  imports = [
    ./home/alacritty.nix
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
    jq
    ncdu
    ripgrep
    tree
  ];

  myme.alacritty = {
    background_opacity = 0.95;
    font_size = 6.0;
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
    htop.enable = true;
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
