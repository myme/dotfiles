{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.myme.dev.vscode;

in
{
  options.myme.dev.vscode = {
    enable = lib.mkEnableOption "Enable VS Code";
  };

  config = {
    programs.vscode = {
      enable = cfg.enable;
      profiles.default.extensions = [
        pkgs.vscode-extensions.dracula-theme.theme-dracula
        pkgs.vscode-extensions.github.copilot
        pkgs.vscode-extensions.github.copilot-chat
        pkgs.vscode-extensions.mkhl.direnv
        pkgs.vscode-extensions.ms-pyright.pyright
        pkgs.vscode-extensions.ms-python.python
        pkgs.vscode-extensions.rust-lang.rust-analyzer
        pkgs.vscode-extensions.vadimcn.vscode-lldb
        pkgs.vscode-extensions.vscodevim.vim
      ];
      # haskell.enable = config.myme.dev.haskell.enable;
    };
  };
}
