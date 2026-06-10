{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.myme.emacs;
  emacsPkg = config.programs.emacs.finalPackage;
  emacsclient = "${emacsPkg}/bin/emacsclient";
  emacsIcon = "${pkgs.emacs}/Applications/Emacs.app/Contents/Resources/Emacs.icns";

  # Build a minimal Spotlight-discoverable .app bundle.
  # `exec` is the body of a /bin/sh script that becomes CFBundleExecutable.
  mkDarwinApp =
    {
      name,
      bundleId,
      exec,
      extraPlist ? "",
    }:
    pkgs.runCommandLocal "${name}.app" { } ''
      app="$out/Applications/${name}.app"
      mkdir -p "$app/Contents/MacOS" "$app/Contents/Resources"

      cp "${emacsIcon}" "$app/Contents/Resources/Emacs.icns"

      cat > "$app/Contents/Info.plist" <<PLIST
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      <plist version="1.0">
      <dict>
        <key>CFBundleName</key><string>${name}</string>
        <key>CFBundleDisplayName</key><string>${name}</string>
        <key>CFBundleIdentifier</key><string>${bundleId}</string>
        <key>CFBundleExecutable</key><string>launcher</string>
        <key>CFBundleIconFile</key><string>Emacs</string>
        <key>CFBundlePackageType</key><string>APPL</string>
        <key>CFBundleVersion</key><string>1</string>
        <key>CFBundleShortVersionString</key><string>1.0</string>
        <key>LSUIElement</key><false/>
        ${extraPlist}
      </dict>
      </plist>
      PLIST

      # Nix substitutes `${exec}` at eval time; the quoted heredoc tag
      # stops the build shell from then expanding `$@` (empty in the
      # build) before writing the file.
      cat > "$app/Contents/MacOS/launcher" <<'LAUNCHER'
      #!/bin/sh
      ${exec}
      LAUNCHER
      chmod +x "$app/Contents/MacOS/launcher"
    '';

  emacsClientApp = mkDarwinApp {
    name = "Emacs Client";
    bundleId = "org.nixos.emacs-client";
    exec = ''exec ${emacsclient} -c -a "" "$@"'';
  };

  # macOS dispatches `org-protocol://` URLs to apps via Apple Events, not
  # argv, so a shell-script CFBundleExecutable can't receive them.
  # AppleScript's `on open location` is the standard no-native-code path.
  # Compiled via /usr/bin/osacompile at activation time (osacompile isn't
  # in nixpkgs).
  orgCaptureScript = pkgs.writeText "org-capture.applescript" ''
    on run
      do shell script "${emacsclient} -c -a \"\""
    end run

    on open location this_URL
      do shell script "${emacsclient} -c -a \"\" " & quoted form of this_URL
    end open location
  '';

in
{
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isDarwin) {
    # GUI launches on macOS (Spotlight, Dock, Finder) inherit launchd's
    # environment, not the login shell's. Without these:
    #  (a) Spotlight-launched Emacs misses DOOMLOCALDIR and can't find
    #      Doom's profile manifest (`void-variable doom-modules`).
    #  (b) The services.emacs daemon agent has no EnvironmentVariables
    #      set by upstream — same problem at login.
    launchd.agents = {
      # (a) Export the vars into the user launchd domain at login.
      doom-env = {
        enable = true;
        config = {
          Label = "org.nixos.doom-env";
          RunAtLoad = true;
          KeepAlive = false;
          ProgramArguments = [
            "/bin/sh"
            "-c"
            ''
              launchctl setenv DOOMLOCALDIR "$HOME/.cache/doomemacs/" ; \
              launchctl setenv DOOMPROFILELOADFILE "$HOME/.cache/doomemacs/load.el"
            ''
          ];
        };
      };

      # (b) Belt-and-braces: also set them directly on the daemon plist,
      # so it doesn't depend on doom-env's RunAtLoad ordering.
      emacs.config = {
        EnvironmentVariables = {
          DOOMLOCALDIR = "${config.home.homeDirectory}/.cache/doomemacs/";
          DOOMPROFILELOADFILE = "${config.home.homeDirectory}/.cache/doomemacs/load.el";
        };
        # Disable launchd's auto-restart so it doesn't race with ec's
        # `-a ""` fallback when the user kills the daemon. With this off,
        # any kill (clean or signal) leaves the daemon dead until the
        # next ec / login.
        KeepAlive = lib.mkForce false;
        # Launch the daemon from inside Emacs.app, not bin/emacs.
        # A bare Unix binary has no Info.plist, so macOS assigns the
        # daemon a non-`.regular` NSApplicationActivationPolicy — the
        # WindowServer then refuses to transfer key-window status to
        # it (you see frames, mouse works, but keyboard goes to the
        # previously frontmost app, and Dock icon bounces forever).
        # Launching from inside the .app bundle gives the daemon
        # proper LaunchServices identity so AppleScript `tell
        # application "Emacs" to activate` works.
        ProgramArguments = lib.mkForce [
          "/bin/sh"
          "-c"
          "/bin/wait4path /nix/store && exec ${emacsPkg}/Applications/Emacs.app/Contents/MacOS/Emacs --fg-daemon"
        ];
      };
    };

    home.packages = [ emacsClientApp ];

    # Darwin equivalent of the Linux org-capture xdg.desktopEntry in
    # default.nix: an .app bundle that handles org-protocol:// URLs and
    # is launchable from Spotlight.
    home.activation.orgCaptureApp = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      app="$HOME/Applications/Home Manager Apps/Org Capture.app"
      run mkdir -p "$HOME/Applications/Home Manager Apps"
      run rm -rf "$app"
      run /usr/bin/osacompile -o "$app" ${orgCaptureScript}
      # Replace osacompile's default droplet icon with the Emacs icon.
      run cp "${emacsIcon}" "$app/Contents/Resources/applet.icns"
      run /usr/libexec/PlistBuddy \
        -c 'Add :CFBundleURLTypes array' \
        -c 'Add :CFBundleURLTypes:0 dict' \
        -c 'Add :CFBundleURLTypes:0:CFBundleURLName string Org Protocol' \
        -c 'Add :CFBundleURLTypes:0:CFBundleURLSchemes array' \
        -c 'Add :CFBundleURLTypes:0:CFBundleURLSchemes:0 string org-protocol' \
        -c 'Add :CFBundleIdentifier string org.nixos.org-capture' \
        "$app/Contents/Info.plist"
      # Bump the bundle mtime so Launch Services re-indexes it (copyApps
      # preserves the nix-store epoch mtime otherwise).
      run /usr/bin/touch "$app"
    '';
  };
}
