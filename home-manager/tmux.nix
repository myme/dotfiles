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
      terminal = "tmux-256color";
      customPaneNavigationAndResize = true;
      # resizeAmount = 5;
      extraConfig = ''
        # True color (24-bit) support
        # https://gist.github.com/andersevenrud/015e61af2fd264371032763d4ed965b6#tmux
        set -ag terminal-overrides ",xterm-256color:RGB"

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
