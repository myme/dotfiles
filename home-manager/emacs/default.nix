{
  config,
  lib,
  pkgs,
  osConfig,
  ...
}:

let
  cfg = config.myme.emacs;
  doom = pkgs.writeShellScriptBin "doom" ''
    ~/.emacs.d/bin/doom "$@"
  '';
  # On Darwin we manage the daemon ourselves (no systemd socket activation),
  # so fall back to `-a ""` to spawn a daemon if one isn't running.
  # Note: regular `"..."` string — `''..''` strips the leading space.
  emacsclientFallback = lib.optionalString pkgs.stdenv.isDarwin " -a \"\"";
  # Use an absolute path so the wrappers work outside a shell-derived PATH
  # (AppleScript `do shell script`, launchd plists, etc.).
  emacsclientBin = "${config.programs.emacs.finalPackage}/bin/emacsclient";
  ec = pkgs.writeShellScriptBin "ec" ''
    ${emacsclientBin} -c${emacsclientFallback} "$@"
  '';
  et = pkgs.writeShellScriptBin "et" ''
    ${emacsclientBin} -t${emacsclientFallback} "$@"
  '';
  # FIXME: Hack to avoid hang on gpg save: https://dev.gnupg.org/T6481
  epg =
    if lib.versionOlder pkgs.gnupg.version "2.4.4" then
      (pkgs.writeShellScriptBin "epg" ''
        PATH="${pkgs.gnupg24}/bin:$PATH" emacs "$@"
      '')
    else
      null;
  inherit (osConfig.myme.machine) flavor;
  deVariant = osConfig.myme.machine.de.variant;
  isWayland =
    flavor == "wsl"
    || builtins.elem deVariant [
      "gnome"
      "hyprland"
    ];
  EDITOR = if osConfig.myme.machine.role == "server" then "${et}/bin/et" else "${ec}/bin/ec";
  xclip-to-org = pkgs.writeShellScriptBin "xclip-to-org" (builtins.readFile ./xclip-to-org.sh);

in
{
  imports = [ ./darwin.nix ];

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
    home = {
      # Doom Emacs (.emacs.d)
      file.".emacs.d".source = pkgs.myme.doomemacs;

      # Doom Emacs local files (~/.cache/doom)
      sessionVariables = lib.mkMerge [
        {
          DOOMLOCALDIR = "~/.cache/doomemacs/";
          DOOMPROFILELOADFILE = "~/.cache/doomemacs/load.el";
        }
        (lib.mkIf cfg.default-editor {
          inherit EDITOR;
        })
      ];

      # Additional packages
      packages =
        with pkgs;
        [
          (aspellWithDicts (
            dicts: with dicts; [
              en
              en-computers
              it
              nb
            ]
          ))
          doom
          ec
          et
          mermaid-cli
          xclip-to-org
        ]
        ++ (if epg != null then [ epg ] else [ ]);
    };

    # Doom Emacs configuration (~/.config/doom)
    xdg.configFile.doom.source = pkgs.stdenv.mkDerivation {
      name = "doom-emacs-src";
      src = ./doom;
      doomConfigExtra = cfg.configExtra;
      inherit (cfg) backgroundOpacity;
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

    xdg.desktopEntries = lib.mkIf pkgs.stdenv.isLinux {
      org-capture = {
        name = "Org Capture";
        genericName = "Emacs Org-Mode Capture";
        exec = "${EDITOR} %u";
        icon = "emacs";
        terminal = false;
        categories = [
          "Development"
          "TextEditor"
        ];
        mimeType = [ "x-scheme-handler/org-protocol" ];
      };
    };
  };
}
