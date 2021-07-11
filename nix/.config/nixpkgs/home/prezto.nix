{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.prezto;
  prezto = pkgs.zsh-prezto;
  starship = pkgs.starship;
  runcoms = "${prezto}/share/zsh-prezto/runcoms";
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
      envExtra     = ''source ${runcoms}/zshenv'';
      initExtraBeforeCompInit = ''
        if [[ $TERM == "dumb" ]]; then
          unsetopt zle
          PS1='$ '
          return
        fi
      '';
      initExtra    = (mkMerge [
        ''
          source ${runcoms}/zshrc

          # Disable/remove right prompt
          export RPS1=""
        ''
      ]);
      loginExtra   = ''source ${runcoms}/zlogin'';
      logoutExtra  = ''source ${runcoms}/zlogout'';
      profileExtra = ''source ${runcoms}/zprofile'';
    };
  };
}
