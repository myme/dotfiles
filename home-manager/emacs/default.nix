{ config, lib, pkgs, specialArgs, ... }:

let
  cfg = config.myme.emacs;
  ec = (pkgs.writeShellScriptBin "ec" ''
    emacsclient -c "$@"
  '');
  et = (pkgs.writeShellScriptBin "et" ''
    emacsclient -t "$@"
  '');

in {
  options.myme.emacs = {
    enable = lib.mkEnableOption "Emacs";
    font = {
      family = lib.mkOption {
        type = lib.types.str;
        default = "NotoSansMono Nerd Font";
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
    home.sessionVariables = {
      DOOMLOCALDIR = "~/.cache/doomemacs/";
      DOOMPROFILELOADFILE = "~/.cache/doomemacs/load.el";
      EDITOR = if specialArgs.nixosConfig.myme.machine.role == "server" then
        "${et}/bin/et"
      else
        "${ec}/bin/ec";
    };

    # Doom Emacs configuration (~/.config/doom)
    xdg.configFile.doom.source = pkgs.stdenv.mkDerivation {
      name = "doom-emacs-src";
      src = ./doom;
      doomFontFamily = pkgs.lib.strings.escapeNixString cfg.font.family;
      doomFontSize = cfg.font.size;
      doomTheme = cfg.theme;
      installPhase = ''
        cp -av $src $out
      '';
      postFixup = ''
        substituteInPlace $out/config.el \
          --subst-var doomFontFamily \
          --subst-var doomFontSize \
          --subst-var doomTheme
      '';
    };

    # Stock emacs
    programs.emacs = {
      enable = lib.mkDefault true;
      extraPackages = epkgs: with epkgs; [
        vterm
      ];
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
      (aspellWithDicts (dicts: with dicts; [
        en
        en-computers
        it
        nb
      ]))
      ec
      et
      nodePackages.mermaid-cli
    ];

    xdg.desktopEntries = {
      org-capture = {
        name = "Org Capture";
        genericName = "Emacs Org-Mode Capture";
        exec = "ec %u";
        icon = "emacs";
        terminal = false;
        categories = [ "Development" "TextEditor" ];
        mimeType = [ "x-scheme-handler/org-protocol" ];
      };
    };
  };
}
