f: { pkgs, ... }: (f {
  imports = [
    ./dev.nix
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
    emacs = {
      enable = true;
      extraPackages = epkgs: with epkgs; [
        vterm
      ];
    };
    fzf.enable = true;
    htop = {
      enable = true;
      settings = {
        left_meters = [ "LeftCPUs2" "Memory" "Swap" ];
        right_meters = [ "RightCPUs2" "Tasks" "LoadAverage" "Uptime" ];
      };
    };
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

  services = {
    emacs = {
      enable = true;
      client.enable = true;
    };
  };
})
