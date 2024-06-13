# Misc Gnome settings
#
# Primarily keybindings for Gnome Shell and more
#
# It's not always trivial to find where a binding is bound. Using `gsettings`
# it's possible to list all bindings and their values.
#
# Example:
#
#   for schema in $(gsettings list-schemas | grep -E 'keybindings|media-keys')
#   do
#       gsettings list-recursively $schema
#   done

{ lib, specialArgs, ... }:

let
  gnome = specialArgs.nixosConfig.services.xserver.desktopManager.gnome;
in
{
  config = lib.mkIf gnome.enable {
    dconf.settings = {
      # Gnome interface
      "org/gnome/desktop/interface" = {
        enable-animations = false;
      };

      # Preferences
      "org/gnome/desktop/wm/preferences" = {
        # Workspaces
        num-workspaces = 10;
      };

      # Keybindings
      "org/gnome/desktop/wm/keybindings" = {
        # Move windows to workspace
        move-to-workspace-1 = [ "<Shift><Super>1" ];
        move-to-workspace-2 = [ "<Shift><Super>2" ];
        move-to-workspace-3 = [ "<Shift><Super>3" ];
        move-to-workspace-4 = [ "<Shift><Super>4" ];
        move-to-workspace-5 = [ "<Shift><Super>5" ];
        move-to-workspace-6 = [ "<Shift><Super>6" ];
        move-to-workspace-7 = [ "<Shift><Super>7" ];
        move-to-workspace-8 = [ "<Shift><Super>8" ];
        move-to-workspace-9 = [ "<Shift><Super>9" ];
        move-to-workspace-10 = [ "<Shift><Super>0" ];

        # Relative move
        move-to-workspace-left = [ "<Shift><Super>p" ];
        move-to-workspace-right = [ "<Shift><Super>n" ];

        # Switch to workspace
        switch-to-workspace-1 = [ "<Super>1" ];
        switch-to-workspace-2 = [ "<Super>2" ];
        switch-to-workspace-3 = [ "<Super>3" ];
        switch-to-workspace-4 = [ "<Super>4" ];
        switch-to-workspace-5 = [ "<Super>5" ];
        switch-to-workspace-6 = [ "<Super>6" ];
        switch-to-workspace-7 = [ "<Super>7" ];
        switch-to-workspace-8 = [ "<Super>8" ];
        switch-to-workspace-9 = [ "<Super>9" ];
        switch-to-workspace-10 = [ "<Super>0" ];

        # Relative switch
        switch-to-workspace-left = [ "<Super>p" ];
        switch-to-workspace-right = [ "<Super>n" ];

        # Disable input source switching bindings
        switch-input-source = [ ];
        switch-input-source-backward = [ ];
      };

      # Mutter
      "org/gnome/mutter" = {
        check-alive-timeout = 60000;
        dynamic-workspaces = false;
      };

      # Gnome shell
      "org/gnome/shell/keybindings" = {
        # Disable default bindings
        focus-active-notification = [ ];
        # I don't wanna launch pinned apps
        switch-to-application-1 = [ ];
        switch-to-application-2 = [ ];
        switch-to-application-3 = [ ];
        switch-to-application-4 = [ ];
        switch-to-application-5 = [ ];
        switch-to-application-6 = [ ];
        switch-to-application-7 = [ ];
        switch-to-application-8 = [ ];
        switch-to-application-9 = [ ];
        switch-to-application-10 = [ ];
      };
    };
  };
}
