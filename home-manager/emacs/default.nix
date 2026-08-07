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
  # Use an absolute path so the wrappers work outside a shell-derived PATH
  # (AppleScript `do shell script`, launchd plists, etc.).
  emacsclientBin = "${config.programs.emacs.finalPackage}/bin/emacsclient";
  emacsAppBin = "${config.programs.emacs.finalPackage}/Applications/Emacs.app/Contents/MacOS/Emacs";

  # Darwin has no systemd socket activation, so the wrappers have to bring
  # the daemon up themselves. `emacsclient -a ""` is the obvious way and is
  # wrong here: it execs a bare `emacs` off PATH, and a binary started
  # outside an .app bundle gets no LaunchServices identity — which silently
  # breaks frame focus (see doom/config.el for the full story). Kick the
  # launchd agent instead, whose ProgramArguments point into Emacs.app, and
  # only start the bundle directly if the agent is missing or wedged.
  ensureDaemon = lib.optionalString pkgs.stdenv.isDarwin ''
    serverUp() { ${emacsclientBin} --eval t >/dev/null 2>&1; }
    if ! serverUp; then
      launchctl kickstart "gui/$(id -u)/org.nix-community.home.emacs" >/dev/null 2>&1 || true
      n=0
      until serverUp || [ "$n" -ge 60 ]; do
        n=$((n + 1))
        sleep 0.5
      done
      serverUp || ${emacsAppBin} --daemon
    fi
  '';
  ec = pkgs.writeShellScriptBin "ec" ''
    ${ensureDaemon}
    exec ${emacsclientBin} -c "$@"
  '';
  et = pkgs.writeShellScriptBin "et" ''
    ${ensureDaemon}
    exec ${emacsclientBin} -t "$@"
  '';
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
    clientWrapper = lib.mkOption {
      type = lib.types.str;
      internal = true;
      readOnly = true;
      default = "${ec}/bin/ec";
      description = ''
        Path to the graphical emacsclient wrapper. Exposed so darwin.nix can
        reuse it in the .app bundles it builds rather than duplicating the
        daemon-startup logic.
      '';
    };
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
      packages = with pkgs; [
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
      ];
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
