overrides: { lib, pkgs, ... }@args: ({
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

  config = lib.mkMerge [{
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
    ] ++ (let
      git-sync = pkgs.callPackage ./git-sync {};
    in [
      git-sync
    ]);

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

    services = {
      emacs = {
        enable = true;
        client.enable = true;
      };
    };
  } (
    if builtins.isFunction overrides
    then overrides args
    else overrides
  )];
})
