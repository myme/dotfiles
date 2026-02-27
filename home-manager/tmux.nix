{ pkgs, ... }: {
  config = {
    home.packages = with pkgs; [
      tmux-xpanes
    ];

    home.sessionVariables = {
      TMUX_XPANES_DEFAULT_CMD = "bat --style=plain --paging=never";
    };

    programs.tmux = {
      enable = true;
      keyMode = "vi";
      shortcut = "a";
      baseIndex = 1;
      escapeTime = 0;
      terminal = "tmux-256color";
      customPaneNavigationAndResize = true;
      # resizeAmount = 5;
      extraConfig = ''
        # True color (24-bit) support
        # https://gist.github.com/andersevenrud/015e61af2fd264371032763d4ed965b6#tmux
        set -ag terminal-overrides ",xterm-256color:RGB"

        # Split bindings
        bind | split-window -h -c '#{pane_current_path}'
        bind - split-window -v -c '#{pane_current_path}'

        # Window bindings
        bind N swap-window -d -t +1
        bind P swap-window -d -t -1
        bind W choose-window -w 'swap-window -d -t %%'

        # Mouse support
        set -g mouse on

        # Sonnette: jump to alerting pane and clear notification
        bind b run-shell '${pkgs.myme.pkgs.sonnette}/bin/sonnette jump'
        bind B run-shell '${pkgs.myme.pkgs.sonnette}/bin/sonnette clear'

        # Keep server running
        set -g exit-empty off
      '';
      plugins = with pkgs.tmuxPlugins; [
        copycat
        {
          plugin = power-theme;
          extraConfig = "set -g @tmux_power_theme 'violet'";
        }
        {
          plugin = sensible;
          extraConfig = ''
            # Sonnette: append notification indicator after tmux-power status-right
            set -ag status-right "#[bg=colour208,fg=colour0,bold]#(${pkgs.myme.pkgs.sonnette}/bin/sonnette status)#[default]"
          '';
        }
        sessionist
        urlview
        yank
      ];
    };
  };
}
