{ pkgs, ... }: {
  config = {
    home.packages = with pkgs; [
      tmux-xpanes
    ];

    programs.tmux = {
      enable = true;
      keyMode = "vi";
      shortcut = "a";
      baseIndex = 1;
      escapeTime = 0;
      terminal = "screen-256color";
      customPaneNavigationAndResize = true;
      # resizeAmount = 5;
      extraConfig = ''
        bind | split-window -h -c '#{pane_current_path}'
        bind - split-window -v -c '#{pane_current_path}'

        # Mouse support
        set -g mouse on

        # Keep server running
        set -g exit-empty off
      '';
      plugins = with pkgs.tmuxPlugins; [
        copycat
        {
          plugin = power-theme;
          extraConfig = "set -g @tmux_power_theme 'violet'";
        }
        sensible
        sessionist
        urlview
        yank
      ];
    };
  };
}
