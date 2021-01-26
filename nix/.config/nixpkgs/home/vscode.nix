{ config, lib, pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      ms-python.python
      ms-vscode.cpptools
      ms-vscode-remote.remote-ssh
      vscodevim.vim
    ] ++ (pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "GitLens";
        publisher = "eamodio";
        version = "11.1.3";
        sha256 = "1x9bkf9mb56l84n36g3jmp3hyfbyi8vkm2d4wbabavgq6gg618l6";
      }
      {
        name = "rust";
        publisher = "rust-lang";
        version = "0.7.8";
        sha256 = "039ns854v1k4jb9xqknrjkj8lf62nfcpfn0716ancmjc4f0xlzb3";
      }
    ]);
    userSettings = {
      "files.watcherExclude" = {
        "**/node_modules/*/**" = true;
        "**/_build/*" = true;
      };
      "rust-client.disableRustup" = true;
      "workbench.colorTheme" = "Dracula";
    };
  };
}
