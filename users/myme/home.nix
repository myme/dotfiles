{ pkgs, ... }: {
  imports = [
    ./alacritty
    ./git.nix
    ./tmux.nix
    ./vim.nix
    ./wm
  ];

  home.packages = with pkgs; [
    fd
    nixfmt
    ripgrep
    tree
  ];

  home.keyboard = {
    layout = "us";
    variant = "alt-intl-unicode";
  };

  # Alacritty
  myme.alacritty = {
    enable = true;
    background_opacity = 0.95;
    font_size = 6.0;
    theme = "dracula";
  };

  # Window manager
  myme.wm = {
    enable = true;
    variant = "xmonad";
  };

  programs = {
    bat.enable = true;
    bash.enable = true;
    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
        enableFlakes = true;
      };
    };
    emacs.enable = true;
    fzf.enable = true;
    rofi = {
      enable = true;
      theme = ./rofi/dracula.rasi;
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
