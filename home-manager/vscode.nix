{ config, lib, pkgs, ... }:

let
  cfg = config.myme.dev.vscode;

in {
  options.myme.dev.vscode = {
    enable = lib.mkEnableOption "Enable VS Code";
  };

  config = {
    programs.vscode = {
      enable = cfg.enable;
      extensions = [
        pkgs.vscode-extensions.mkhl.direnv
        pkgs.vscode-extensions.rust-lang.rust-analyzer
        pkgs.vscode-extensions.vscodevim.vim
      ];
      # haskell.enable = config.myme.dev.haskell.enable;
    };
  };
}
