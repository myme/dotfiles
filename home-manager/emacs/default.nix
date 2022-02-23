{ pkgs, ... }:
{
  config = {
    # Doom Emacs (.emacs.d)
    home.file.".emacs.d".source = pkgs.myme.doom-emacs;

    # Doom Emacs local files (~/.cache/doom)
    home.sessionVariables.DOOMLOCALDIR = "~/.cache/doom-emacs/";

    # Doom Emacs configuration (~/.config/doom)
    xdg.configFile.doom.source = ./doom;

    # Stock emacs
    programs.emacs = {
      enable = true;
      extraPackages = epkgs: with epkgs; [
        vterm
      ];
    };

    services = {
      emacs = {
        enable = true;
        client.enable = true;
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
      nodePackages.mermaid-cli
    ];
  };
}
