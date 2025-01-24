{ config, lib, pkgs, specialArgs, ... }:

let
  cfg = config.myme.emacs;
  doom = (pkgs.writeShellScriptBin "doom" ''
    ~/.emacs.d/bin/doom "$@"
  '');
  ec = (pkgs.writeShellScriptBin "ec" ''
    emacsclient -c "$@"
  '');
  et = (pkgs.writeShellScriptBin "et" ''
    emacsclient -t "$@"
  '');
  # FIXME: Hack to avoid hang on gpg save: https://dev.gnupg.org/T6481
  epg = if lib.versionOlder pkgs.gnupg.version "2.4.4" then (pkgs.writeShellScriptBin "epg" ''
    PATH="${pkgs.gnupg24}/bin:$PATH" emacs "$@"
  '') else null;
  flavor = specialArgs.nixosConfig.myme.machine.flavor;
  deVariant = specialArgs.nixosConfig.myme.machine.de.variant;
  isWayland = flavor == "wsl" || builtins.elem deVariant [ "gnome" "hyprland" ];
  EDITOR = if specialArgs.nixosConfig.myme.machine.role == "server" then
    "${et}/bin/et"
  else
    "${ec}/bin/ec";
  xclip-to-org = pkgs.writeShellScriptBin "xclip-to-org" (builtins.readFile ./xclip-to-org.sh);

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
      nodeExecutable = "${pkgs.nodejs}/bin/node";
      doomConfigExtra = cfg.configExtra;
      doomFontFamily = pkgs.lib.strings.escapeNixString cfg.font.family;
      doomFontSize = cfg.font.size;
      doomTheme = cfg.theme;
      installPhase = ''
        cp -av $src $out
      '';
      postFixup = ''
        substituteInPlace $out/config.el \
          --subst-var nodeExecutable \
          --subst-var doomConfigExtra \
          --subst-var doomFontFamily \
          --subst-var doomFontSize \
          --subst-var doomTheme
      '';
    };

    # Stock emacs
    programs.emacs = {
      enable = lib.mkDefault true;
      package = if isWayland then pkgs.emacs29-pgtk else pkgs.emacs29;
      extraPackages = epkgs: with epkgs; [ vterm ];
    };

    services = {
      emacs = {
        enable = true;
        client.enable = true;
        socketActivation.enable = true;
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
    ] ++ (if epg != null then [epg] else []);

    xdg.desktopEntries = {
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
