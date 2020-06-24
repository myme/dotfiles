{ config, lib, pkgs, ... }:
{
  config = {
    programs.tmux = {
      enable = true;
      keyMode = "vi";
      shortcut = "a";
      baseIndex = 1;
      escapeTime = 0;
      terminal = "screen-256color";
      customPaneNavigationAndResize = true;
      resizeAmount = 5;
      extraConfig = ''
        # Spawn new shells from the current working directory
        bind | split-window -h -c '#{pane_current_path}'
        bind - split-window -v -c '#{pane_current_path}'

        # Disable automatic window rename
        setw -g allow-rename off
        setw -g automatic-rename off

        # Look and feel
        set -g pane-border-style "fg=blue,bg=default"
        set -g message-style "fg=white,bg=black,bright"
        set -g status-style "fg=blue,bg=default"
        set -g status-justify "centre"
      '';
      plugins = with pkgs.tmuxPlugins; [
        sensible
        sessionist
        yank
      ];
    };

    programs.zsh.shellAliases = {
      ta = "tmux attach -t";
      tl = "tmux list-sessions";
    };
  };
}
