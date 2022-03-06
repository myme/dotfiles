{ lib, pkgs, ... }: ({
  imports = [
    ./barrier.nix
    ./dev.nix
    ./emacs
    ./git.nix
    ./nixon.nix
    ./tmux.nix
    ./vim.nix
    ./wm
  ];

  config = {
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
      myme.apps.git-sync
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
      fish.enable = true;
      fzf = {
        enable = true;
        fileWidgetCommand = "fd --type f";
        fileWidgetOptions = ["--preview 'head {}'"];
        changeDirWidgetCommand = "fd --type d";
        changeDirWidgetOptions = ["--preview 'tree -C {} | head -200'"];
      };
      htop = {
        enable = true;
        settings = {
          left_meters = [ "LeftCPUs2" "Memory" "Swap" ];
          right_meters = [ "RightCPUs2" "Tasks" "LoadAverage" "Uptime" ];
        };
      };
      nixon = {
        enable = true;
        source_dirs = [
          "~/nixos"
          "~/notes"
          "~/src"
        ];
        exact_match = true;
        ignore_case = true;
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
  };
})
