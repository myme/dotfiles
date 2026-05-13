{ config, lib, pkgs, osConfig, ... }:

let
  cfg = config.myme.emacs;
  doom = (pkgs.writeShellScriptBin "doom" ''
    ~/.emacs.d/bin/doom "$@"
  '');
  # On Darwin we manage the daemon ourselves (no systemd socket activation),
  # so fall back to `-a ""` to spawn a daemon if one isn't running.
  # Note: regular `"..."` string — `''..''` strips the leading space.
  emacsclientFallback = lib.optionalString pkgs.stdenv.isDarwin " -a \"\"";
  ec = (pkgs.writeShellScriptBin "ec" ''
    emacsclient -c${emacsclientFallback} "$@"
  '');
  et = (pkgs.writeShellScriptBin "et" ''
    emacsclient -t${emacsclientFallback} "$@"
  '');
  # FIXME: Hack to avoid hang on gpg save: https://dev.gnupg.org/T6481
  epg = if lib.versionOlder pkgs.gnupg.version "2.4.4" then (pkgs.writeShellScriptBin "epg" ''
    PATH="${pkgs.gnupg24}/bin:$PATH" emacs "$@"
  '') else null;
  flavor = osConfig.myme.machine.flavor;
  deVariant = osConfig.myme.machine.de.variant;
  isWayland = flavor == "wsl" || builtins.elem deVariant [ "gnome" "hyprland" ];
  EDITOR = if osConfig.myme.machine.role == "server" then
    "${et}/bin/et"
  else
    "${ec}/bin/ec";
  xclip-to-org = pkgs.writeShellScriptBin "xclip-to-org" (builtins.readFile ./xclip-to-org.sh);

  # Build a minimal Spotlight-discoverable .app bundle on Darwin.
  # `exec` is the body of a /bin/sh script that becomes CFBundleExecutable.
  mkDarwinApp = { name, bundleId, exec, extraPlist ? "" }:
    pkgs.runCommandLocal "${name}.app" {} ''
      app="$out/Applications/${name}.app"
      mkdir -p "$app/Contents/MacOS" "$app/Contents/Resources"

      cp "${pkgs.emacs}/Applications/Emacs.app/Contents/Resources/Emacs.icns" \
         "$app/Contents/Resources/Emacs.icns"

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
    exec = ''exec ${pkgs.emacs}/bin/emacsclient -c -a "" "$@"'';
  };

in {
  options.myme.emacs = {
    enable = lib.mkEnableOption "Emacs";
    configExtra = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Additional commands to add to config.el";
    };
    default-editor = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Set emacs as $EDITOR";
    };
    font = {
      family = lib.mkOption {
        type = lib.types.str;
        default = "Noto Mono for Powerline";
        description = "Doom font family";
      };
      size = lib.mkOption {
        type = lib.types.int;
        default = 14;
        description = "Doom font size";
      };
    };
    theme = lib.mkOption {
      type = lib.types.str;
      default = "doom-dracula";
      description = "Doom theme";
    };
    backgroundOpacity = lib.mkOption {
      type = lib.types.int;
      default = 95;
      description = "Emacs background opacity";
    };
  };

  config = lib.mkIf cfg.enable {
    # Doom Emacs (.emacs.d)
    home.file.".emacs.d".source = pkgs.myme.doomemacs;

    # Doom Emacs local files (~/.cache/doom)
    home.sessionVariables = lib.mkMerge [
      {
        DOOMLOCALDIR = "~/.cache/doomemacs/";
        DOOMPROFILELOADFILE = "~/.cache/doomemacs/load.el";
      }
      (lib.mkIf cfg.default-editor {
        inherit EDITOR;
      })
    ];

    # Doom Emacs configuration (~/.config/doom)
    xdg.configFile.doom.source = pkgs.stdenv.mkDerivation {
      name = "doom-emacs-src";
      src = ./doom;
      doomConfigExtra = cfg.configExtra;
      backgroundOpacity = cfg.backgroundOpacity;
      doomFontFamily = pkgs.lib.strings.escapeNixString cfg.font.family;
      doomFontSize = cfg.font.size;
      doomTheme = cfg.theme;
      installPhase = ''
        cp -av $src $out
      '';
      postFixup = ''
        substituteInPlace $out/config.el \
          --subst-var backgroundOpacity \
          --subst-var doomConfigExtra \
          --subst-var doomFontFamily \
          --subst-var doomFontSize \
          --subst-var doomTheme
      '';
    };

    # Stock emacs
    programs.emacs = {
      enable = lib.mkDefault true;
      package = if isWayland then pkgs.emacs-pgtk else pkgs.emacs;
      extraPackages = epkgs: with epkgs; [ vterm ];
    };

    services = {
      emacs = {
        enable = true;
        client.enable = true;
        socketActivation.enable = true;
      };
    };

    launchd.agents = lib.mkIf pkgs.stdenv.isDarwin {
      doom-env = {
        enable = true;
        config = {
          Label = "org.nixos.doom-env";
          RunAtLoad = true;
          KeepAlive = false;
          ProgramArguments = [
            "/bin/sh" "-c"
            ''
              launchctl setenv DOOMLOCALDIR "$HOME/.cache/doomemacs/" ; \
              launchctl setenv DOOMPROFILELOADFILE "$HOME/.cache/doomemacs/load.el"
            ''
          ];
        };
      };

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
      };
    };

    # Additional packages
    home.packages = with pkgs; [
      (aspellWithDicts (dicts: with dicts; [ en en-computers it nb ]))
      doom
      ec
      et
      nodePackages.mermaid-cli
      xclip-to-org
    ] ++ (if epg != null then [epg] else [])
      ++ lib.optional pkgs.stdenv.isDarwin emacsClientApp;

    xdg.desktopEntries = lib.mkIf pkgs.stdenv.isLinux {
      org-capture = {
        name = "Org Capture";
        genericName = "Emacs Org-Mode Capture";
        exec = "${EDITOR} %u";
        icon = "emacs";
        terminal = false;
        categories = [ "Development" "TextEditor" ];
        mimeType = [ "x-scheme-handler/org-protocol" ];
      };
    };
  };
}
