{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.myme.dev;

in {
  options.myme.dev = {
    # Haskell options
    haskell = {
      enable = mkEnableOption "Enable Haskell development tools";
    };

    # Nodejs options
    nodejs = {
      enable = mkEnableOption "Enable JavaScript development tools";
    };

    # Python options
    python = {
      enable = mkEnableOption "Enable Python development tools";
    };
  };

  config = {
    home.packages = mkMerge [
      # Haskell
      (mkIf cfg.haskell.enable (with pkgs; [
        haskell-language-server
      ]))

      # Nodejs
      (mkIf cfg.nodejs.enable (with pkgs; [
        nodejs
        nodePackages.typescript-language-server
        nodePackages.prettier
      ]))

      # Python
      (mkIf cfg.python.enable (with pkgs; [
        (python3.withPackages (ps: with ps; [
          black
          ipython
        ]))
        python-language-server
      ]))
    ];
  };
}
