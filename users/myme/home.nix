{ pkgs, ... }: {
  imports = [
    ./git.nix
    ./tmux.nix
    ./vim.nix
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
