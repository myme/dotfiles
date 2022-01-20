f: { pkgs, ... }: (f {
  imports = [
    ./git.nix
    ./nixon.nix
    ./tmux.nix
    ./vim.nix
    ./wm
  ];

  home.packages = with pkgs; [
    dua
    fd
    jq
    lsof
    nixfmt
    ripgrep
    tree
    unzip
    zip
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
    emacs.enable = true;
    fzf.enable = true;
    nixon = {
      enable = true;
      exact_match = true;
      source_dirs = [
        "~/nixos"
        "~/src"
      ];
      use_direnv = true;
      use_nix = true;
    };
    starship = {
      enable = true;
      settings.time = {
        disabled = false;
        format = "[$time]($style) ";
      };
    };
  };
})
