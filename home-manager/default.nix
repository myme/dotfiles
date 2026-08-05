{
  config,
  lib,
  options,
  pkgs,
  osConfig,
  ...
}:

let
  inherit (config.myme) defaultPrograms;
  dockerEnabled = osConfig ? virtualisation && osConfig.virtualisation.docker.enable;

  # home-manager master folded the flat fzf widget settings into nested
  # `fileWidget.{command,options}` submodules, keeping the old names as
  # renamed-option aliases. `home-manager-stable` (release-26.05) only has
  # the flat ones, so declare the values once and shape them to whichever
  # generation a machine is built against — using the flat names everywhere
  # would work, but warns on every rebuild of an unstable machine.
  fzfWidgets = {
    file = {
      command = "fd --type f";
      options = [ "--preview 'bat {}'" ];
    };
    changeDir = {
      command = "fd --type d";
      options = [ "--preview 'tree -C {} | head -200'" ];
    };
  };

in
{
  imports = [
    ./btop.nix
    ./dev.nix
    ./emacs
    ./fonts.nix
    ./ghostty.nix
    ./git.nix
    ./irc.nix
    ./nixon
    ./spotify.nix
    ./tmux.nix
    ./vim
    ./wm
    ./wsl.nix
  ];

  options.myme.defaultPrograms = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = "Include a default set of programs and services.";
  };

  config = {
    home = {
      packages = lib.mkIf defaultPrograms (
        with pkgs;
        [
          dua
          fd
          jq
          lsof
          nix-diff
          nix-tree
          nixfmt
          ripgrep
          tree
          unzip
          xh
          yq
          zip
          annodate
          myme.pkgs.git-sync
          myme.pkgs.hm
        ]
      );

      keyboard = {
        layout = "us";
        variant = "alt-intl-unicode";
      };

      # Generic shell aliases
      shellAliases = {
        dc = "docker compose";
        la = "ls -la";
        ls = "ls --color=auto";
        ll = "ls -l";
        ta = "tmux attach-session";
      };
    };

    # But of course!
    myme.emacs.enable = lib.mkDefault defaultPrograms;
    myme.vim.enable = lib.mkDefault defaultPrograms;

    programs = {
      bat.enable = lib.mkDefault defaultPrograms;
      bash = {
        enable = true;
        historyControl = [
          "erasedups"
          "ignoredups"
          "ignorespace"
        ];
        initExtra = lib.mkIf dockerEnabled ''
          # Alias completion
          source ${pkgs.complete-alias}/bin/complete_alias

          # Docker
          complete -F _complete_alias dc
        '';
      };
      direnv = {
        enable = lib.mkDefault defaultPrograms;
        enableNushellIntegration = true;
        nix-direnv.enable = true;
        stdlib = builtins.readFile ./direnvrc;
      };
      fish.enable = lib.mkDefault defaultPrograms;
      fzf = {
        enable = lib.mkDefault defaultPrograms;
      }
      // (
        if options.programs.fzf ? fileWidget then
          {
            fileWidget = fzfWidgets.file;
            changeDirWidget = fzfWidgets.changeDir;
          }
        else
          {
            fileWidgetCommand = fzfWidgets.file.command;
            fileWidgetOptions = fzfWidgets.file.options;
            changeDirWidgetCommand = fzfWidgets.changeDir.command;
            changeDirWidgetOptions = fzfWidgets.changeDir.options;
          }
      );
      helix = {
        enable = lib.mkDefault defaultPrograms;
        settings = {
          theme = "dracula";
          editor = {
            line-number = "relative";
          };
        };
      };
      htop = {
        enable = lib.mkDefault defaultPrograms;
        settings = {
          left_meters = [
            "LeftCPUs2"
            "Memory"
            "Swap"
          ];
          right_meters = [
            "RightCPUs2"
            "Tasks"
            "LoadAverage"
            "Uptime"
          ];
        };
      };
      nixon = {
        enable = lib.mkDefault defaultPrograms;
        source_dirs = [
          "~/code/*"
          "~/nixos"
          "~/notes"
          "~/src"
        ];
        exact_match = true;
        ignore_case = true;
        use_direnv = true;
        use_nix = true;
      };
      nushell.enable = lib.mkDefault defaultPrograms;
      ssh = {
        # programs.ssh is enabled transitively. Opt out of the soon-to-be
        # removed implicit defaults, keeping the previous values explicitly so
        # ~/.ssh/config is unchanged and the deprecation warning is silenced.
        enableDefaultConfig = false;
        settings."*" = {
          forwardAgent = false;
          addKeysToAgent = "no";
          compression = false;
          serverAliveInterval = 0;
          serverAliveCountMax = 3;
          hashKnownHosts = false;
          userKnownHostsFile = "~/.ssh/known_hosts";
          controlMaster = "no";
          controlPath = "~/.ssh/master-%r@%n:%p";
          controlPersist = "no";
        };
      };
      starship = {
        enable = lib.mkDefault defaultPrograms;
        settings.time = {
          disabled = false;
          format = "[$time]($style) ";
        };
      };
      zoxide.enable = lib.mkDefault defaultPrograms;
    };
  };
}
