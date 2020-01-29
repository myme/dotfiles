{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.prezto;
  prezto = pkgs.zsh-prezto;
  starship = pkgs.starship;
in

{
  options.programs.prezto = {
    enable = mkEnableOption "prezto - The configuration framework for Zsh";
  };

  config = mkIf cfg.enable {
    home.file.".zpreztorc".text = ''
      # Color output (auto set to 'no' on dumb terminals).
      zstyle ':prezto:*:*' color 'yes'

      # Modules
      zstyle ':prezto:load' pmodule \
        'environment' \
        'terminal' \
        'editor' \
        'history' \
        'directory' \
        'spectrum' \
        'utility' \
        'completion'

      # Keybindings
      zstyle ':prezto:module:editor' key-bindings 'emacs'
    '';

    programs.zsh = with pkgs; {
      envExtra     = ''source ${prezto}/runcoms/zshenv'';
      initExtraBeforeCompInit = ''
        if [[ $TERM == "dumb" ]]; then
          unsetopt zle
          PS1='$ '
          return
        fi
      '';
      initExtra    = (mkMerge [
        ''
          source ${prezto}/runcoms/zshrc

          # Disable/remove right prompt
          export RPS1=""
        ''
      ]);
      loginExtra   = ''source ${prezto}/runcoms/zlogin'';
      logoutExtra  = ''source ${prezto}/runcoms/zlogout'';
      profileExtra = ''source ${prezto}/runcoms/zprofile'';
    };
  };
}
