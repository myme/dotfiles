{ config, lib, pkgs, ... }:

let
  cfg = config.myme.wm;

in {
  config = {
    # XMonad
    xsession.windowManager.xmonad = {
      enable = cfg.variant == "xmonad";
      enableContribAndExtras = true;
      config = ./xmonad.hs;
      libFiles = {
        "FontAwesome.hs" = ./lib/FontAwesome.hs;
      };
    };

  };
}
