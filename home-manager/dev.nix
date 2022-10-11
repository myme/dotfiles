# TODO: Pair programming setup using Docker/Podman + Tmate
# Use a docker/pod with tmate and usual dev tools to share a tmux session with a peer.

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.myme.dev;

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

    # Elm
    elm = {
      enable = mkEnableOption "Enable Elm development tools";
    };

    # Haskell options
    haskell = {
      enable = mkEnableOption "Enable Haskell development tools";
      lsp = mkOption {
        type = types.bool;
        default = true;
        description = "Enable haskell-language-server";
      };
      ormolu = mkOption {
        type = types.bool;
        default = true;
        description = "Install the Ormolu Haskell source code formatter";
      };
    };

    # Nodejs options
    nodejs = {
      enable = mkEnableOption "Enable JavaScript development tools";
      interpreter = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the Node.js interpreter";
      };
    };

    # Python options
    python = {
      enable = mkEnableOption "Enable Python development tools";
      interpreter = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the Python interpreter";
      };
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
        man-pages-posix
      ]))

      # C/C++
      (mkIf cfg.cpp.enable (with pkgs; [
        ccls
      ]))

      # Elm
      (mkIf cfg.elm.enable (with pkgs; [
        elmPackages.elm-language-server
      ]))

      # Haskell
      (mkIf (cfg.haskell.enable) (with pkgs; [
        (mkIf cfg.haskell.lsp haskell-language-server)
        (mkIf cfg.haskell.ormolu ormolu)
      ]))

      # Nodejs
      (mkIf cfg.nodejs.enable (with pkgs; [
        (mkIf cfg.nodejs.interpreter nodejs)
        nodePackages.typescript
        nodePackages.typescript-language-server
        nodePackages.prettier
      ]))

      # Python
      (mkIf cfg.python.enable (with pkgs; [
        (mkIf cfg.python.interpreter (python3.withPackages (ps: with ps; [
          ipython
        ])))
        black
        # python-language-server
        pyright
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

    home.file = mkMerge [
      (mkIf cfg.haskell.enable {
        ".ghci".text = ''
          :set prompt "λ: "
        '';
      })
    ];
  };
}
