{ config, lib, pkgs, ... }:
{
  config = {
    programs.bash = {
      enable = true;
      shellAliases = {
        l = "ls -1A";
        la = "ll -A";
        ll = "ls -lh";
        lr = "ll -R";
        ls = "ls --color=auto --group-directories-first";
      };
    };
  };
}
