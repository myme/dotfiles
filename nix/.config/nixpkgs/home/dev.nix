{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.myme.dev;
  unstable = import <unstable> {};

in {
  options.myme.dev = {
    # Documentation (Man, info, ++)
    docs = {
      enable = mkEnableOption "Enable documentation (man, info, ++)";
    };

    # C/C++ options
    cpp = {
      enable = mkEnableOption "Enable C/C++ development tools";
    };

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

    # GitHub options
    github = {
      enable = mkEnableOption "Enable GitHub development tools";
    };

    # Rust options
    rust = {
      enable = mkEnableOption "Enable Rust development tools";
    };

    # Shell options
    shell = {
      enable = mkEnableOption "Enable Shell script development tools";
    };
  };

  config = {
    programs = {
      info.enable = cfg.docs.enable;
      man = {
        enable = cfg.docs.enable;
        generateCaches = true;
      };
    };
    home.packages = mkMerge [
      # Docs
      (mkIf cfg.docs.enable (with pkgs; [
        man-pages
        posix_man_pages
      ]))

      # C/C++
      (mkIf cfg.cpp.enable (with pkgs; [
        ccls
      ]))

      # Haskell
      (mkIf cfg.haskell.enable (with unstable; [
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

      # GitHub
      (mkIf cfg.github.enable (with pkgs; [
        gitAndTools.gh
      ]))

      # Rust
      (mkIf cfg.rust.enable (with pkgs; [
        rust-analyzer
      ]))

      # Shell
      (mkIf cfg.shell.enable (with pkgs; [
        shellcheck
      ]))
    ];
  };
}
