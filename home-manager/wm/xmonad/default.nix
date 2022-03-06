{ config, lib, pkgs, ... }@args:

let
  cfg = config.myme.wm;
  machine = args.specialArgs.nixosConfig.myme.machine;

in {
  options.myme.wm.xmonad = {
    fontSize = lib.mkOption {
      type = lib.types.int;
      default = 12;
      description = "XMonad font size";
    };
    smartBorder = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "XMonad spacing smart border";
    };
    spaces = lib.mkOption {
      type = lib.types.int;
      default = 20;
      description = "XMonad smart border spacing";
    };
  };

  config = {
    # XMonad
    xsession.windowManager.xmonad = {
      enable = cfg.variant == "xmonad";
      enableContribAndExtras = true;
      config = ./xmonad.hs;
      libFiles = {
        "FontAwesome.hs" = ./lib/FontAwesome.hs;
        "Variables.hs" = pkgs.writeText "Variables.hs" ''
          module Variables where
          fontName = "xft:Dejavu Sans Mono for Powerline:regular:size=${builtins.toString cfg.xmonad.fontSize}:antialias=true:hinting=true"
          smartBorder = ${if cfg.xmonad.smartBorder then "True" else "False"}
          spaces = ${builtins.toString cfg.xmonad.spaces}
        '';
      };
    };

  };
}