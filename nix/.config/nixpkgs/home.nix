{ config, pkgs, ... }:
{
  imports = [
    ./home/alacritty.nix
    ./home/prezto.nix
  ];

  home.packages = with pkgs; [
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
      enableZshIntegration = false;
      settings.time = {
        disabled = false;
        format = "%T";
      };
    };
    vim.enable = true;
    zsh.enable = true;
  };
}
