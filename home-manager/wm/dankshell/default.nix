{
  config,
  lib,
  flake-inputs,
  ...
}:

let
  cfg = config.myme.wm.dankshell;

in
{
  imports = [ flake-inputs.dms.homeModules.dank-material-shell ];

  options.myme.wm.dankshell = {
    enable = lib.mkEnableOption "DankMaterialShell desktop shell";
  };

  config = lib.mkIf cfg.enable {
    # DMS ships its own freedesktop notification daemon + toast popups, so
    # disable dunst to avoid a clash over the org.freedesktop.Notifications
    # D-Bus name. mkForce overrides the unconditional enable in ../default.nix.
    services.dunst.enable = lib.mkForce false;

    programs.dank-material-shell = {
      enable = true;
      systemd.enable = true; # dms.service, bound to the wayland systemd target

      settings = {
        # Don't let DMS draw its own wallpaper layer — defer to the external
        # manager (hyprpaper, configured in the hyprland module). An empty
        # screen list for the "wallpaper" component disables DMS's background.
        screenPreferences = {
          wallpaper = [ ];
        };

        # Don't write matugen theme templates into external apps (emacs,
        # ghostty, alacritty, ...). Their configs are managed read-only by
        # home-manager, so DMS's writes fail (read-only fs) and pop a "Theme
        # worker failed" toast. The shell/bar still themes itself in-process.
        runDmsMatugenTemplates = false;

        # Faithful copy of the upstream default bar (SettingsData.qml barConfigs[0])
        # with position = 1 (Bottom). screenPreferences ["all"] renders one bar per
        # monitor; leftWidgets already carry the workspaces + focused window.
        barConfigs = [
          {
            id = "default";
            name = "Main Bar";
            enabled = true;
            position = 1; # 0 = Top, 1 = Bottom
            screenPreferences = [ "all" ];
            showOnLastDisplay = true;
            leftWidgets = [
              "launcherButton"
              "workspaceSwitcher"
              "focusedWindow"
            ];
            centerWidgets = [
              "music"
              "clock"
              "weather"
            ];
            rightWidgets = [
              "systemTray"
              "clipboard"
              "cpuUsage"
              "memUsage"
              "notificationButton"
              "battery"
              "controlCenterButton"
            ];
            spacing = 0; # 0 = no outer margin -> bar spans edge-to-edge, no bottom gap
            innerPadding = 4;
            bottomGap = 0;
            transparency = 1.0;
            widgetTransparency = 1.0;
            squareCorners = true; # flat corners (no rounded floating pill)
            noBackground = false;
            maximizeWidgetIcons = false;
            maximizeWidgetText = false;
            removeWidgetPadding = false;
            widgetPadding = 8;
            gothCornersEnabled = false;
            gothCornerRadiusOverride = false;
            gothCornerRadiusValue = 12;
            borderEnabled = false;
            borderColor = "surfaceText";
            borderOpacity = 1.0;
            borderThickness = 1;
            widgetOutlineEnabled = false;
            widgetOutlineColor = "primary";
            widgetOutlineOpacity = 1.0;
            widgetOutlineThickness = 1;
            fontScale = 1.0;
            iconScale = 1.0;
            autoHide = false;
            autoHideStrict = false;
            autoHideDelay = 250;
            showOnWindowsOpen = false;
            openOnOverview = false;
            visible = true;
            popupGapsAuto = true;
            popupGapsManual = 4;
            maximizeDetection = true;
            useOverlayLayer = false;
            scrollEnabled = true;
            scrollXBehavior = "column";
            scrollYBehavior = "workspace";
            shadowIntensity = 0;
            shadowOpacity = 60;
            shadowColorMode = "default";
            shadowCustomColor = "#000000";
            clickThrough = false;
          }
        ];
      };
    };
  };
}
