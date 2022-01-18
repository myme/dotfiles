{ ... }: {
  config = {
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
        # set -g mouse on

        # Look and feel
        set -g pane-border-style "fg=blue,bg=default"
        set -g message-style "fg=white,bg=black,bright"
        set -g status-style "fg=blue,bg=default"
        set -g status-justify "centre"
      '';
    };
  };
}
