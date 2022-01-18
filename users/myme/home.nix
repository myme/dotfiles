{ pkgs, ... }: {
  imports = [
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
    starship = {
      enable = true;
      settings.time = {
        disabled = false;
        format = "[$time]($style) ";
      };
    };
  };
}
