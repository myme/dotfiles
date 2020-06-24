{ config, lib, pkgs, ... }:
{
  config = {
    programs.bash = {
      enable = true;
      shellAliases = {
        ll = "ls -l";
        la = "ls -la";
      };
    };
  };
}
