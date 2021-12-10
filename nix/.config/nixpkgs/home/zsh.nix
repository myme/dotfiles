{ config, lib, pkgs, ... }:

{
  config = {
    programs.zsh = {
      enable = true;
      initExtraBeforeCompInit = ''
        if [[ $TERM == "dumb" ]]; then
          unsetopt zle
          PS1='$ '
          return
        fi
      '';
      prezto = {
        enable = true;
      };
    };
  };
}
