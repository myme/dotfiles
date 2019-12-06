{ config, pkgs, ... }:
{
  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;
  };
}
