{ pkgs, ... }:
{
  config = {
    programs.urxvt = {
      enable = true;
      fonts = [
        "xft:Dejavu Sans Mono for Powerline:size=14"
      ];
      keybindings = {
        "Shift-Control-C" = "eval:selection_to_clipboard";
        "Shift-Control-V" = "eval:paste_clipboard";
      };
      transparent = true;
      shading = 10;
    };

    xresources.extraConfig = ''
      URxvt.termName:      xterm-256color

      ! https://github.com/logico-dev/Xresources-themes/blob/1df25bf5b2e639e8695e8f2eb39e9d373af3b888/one-dark.Xresources
      ! Custom
      ! Original theme https://github.com/nathanbuchar/atom-one-dark-terminal
      URxvt.foreground:         #ABB2BF
      URxvt.background:         #1E2127
      URxvt.cursorColor:        #5C6370
      URxvt.highlightColor:     #ffb4cc
      URxvt.highlightTextColor: #cc3e6c

      ! Negro y gris
      URxvt.color0:        #1E2127
      URxvt.color8:        #5C6370
      ! Rojos
      URxvt.color1:        #E06C75
      URxvt.color9:        #E06C75
      ! Verde
      URxvt.color2:        #98C379
      URxvt.color10:       #98C379
      ! Amarillo
      URxvt.color3:        #D19A66
      URxvt.color11:       #D19A66
      ! Azul
      URxvt.color4:        #61AFEF
      URxvt.color12:       #61AFEF
      ! Magenta
      URxvt.color5:        #C678DD
      URxvt.color13:       #C678DD
      ! Cyan
      URxvt.color6:        #56B6C2
      URxvt.color14:       #56B6C2
      ! Blanco
      URxvt.color7:        #ABB2BF
      URxvt.color15:       #FFFFFF
    '';
  };
}
