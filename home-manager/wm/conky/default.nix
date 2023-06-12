{ config, lib, pkgs, ... }:

let
  cfg = config.myme.wm.conky;
  conkyrc = pkgs.stdenv.mkDerivation {
    name = "myme-conkyrc";
    srcs = ./conkyrc;
    dontUnpack = true;
    conkyFontFamily = cfg.font.family;
    conkyFontSize = cfg.font.size;
    conkyMinWidth = cfg.minWidth;
    conkyMaxWidth = cfg.maxWidth;
    installPhase = ''
      cp -av "$srcs" "$out"
    '';
    postFixup = ''
      substituteInPlace $out \
        --subst-var conkyFontFamily \
        --subst-var conkyFontSize \
        --subst-var conkyMinWidth \
        --subst-var conkyMaxWidth
    '';
  };

in {
  options.myme.wm.conky = {
    enable = lib.mkEnableOption "Enable conky resource monitor";
    font = {
      family = lib.mkOption {
        type = lib.types.str;
        default = "Noto Mono for Powerline";
        description = "Conky font family";
      };
      size = lib.mkOption {
        type = lib.types.int;
        default = 10;
        description = "Conky font size";
      };
    };
    minWidth = lib.mkOption {
      type = lib.types.int;
      default = 300;
      description = "Conky minimum width";
    };
    maxWidth = lib.mkOption {
      type = lib.types.int;
      default = 300;
      description = "Conky maximum width";
    };
  };

  config = lib.mkIf cfg.enable {
    # Resource monitor (conky)
    systemd.user.services.conky = {
      Unit = {
        Description = "Conky System Monitor";
        After = "graphical-session-pre.target";
        PartOf = "graphical-session.target";
      };

      Service = {
        ExecStart = "${pkgs.conky}/bin/conky -c ${conkyrc}";
        Restart = "on-failure";
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };
}
