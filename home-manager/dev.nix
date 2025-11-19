# TODO: Pair programming setup using Docker/Podman + Tmate
# Use a docker/pod with tmate and usual dev tools to share a tmux session with a peer.

{ config, lib, pkgs, ... }:

let
  cfg = config.myme.dev;

in {
  imports = [
    ./vscode.nix
  ];

  options.myme.dev = {
    # Documentation (Man, info, ++)
    docs = {
      enable = lib.mkEnableOption "Enable documentation (man, info, ++)";
    };

    # C/C++ options
    cpp = {
      enable = lib.mkEnableOption "Enable C/C++ development tools";
    };

    # LLM support (Claude, Copilot, ...)
    llm = {
      enable = lib.mkEnableOption "Enable LLM editor integrations";
      claude = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable the Claude Code CLI tool";
      };
      copilot = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable the Copilot CLI tool";
      };
      gemini = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable the Gemini CLI tool";
      };
    };

    # Elm
    elm = {
      enable = lib.mkEnableOption "Enable Elm development tools";
    };

    # Haskell options
    haskell = {
      enable = lib.mkEnableOption "Enable Haskell development tools";
      lsp = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable haskell-language-server";
      };
      ormolu = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Install the Ormolu Haskell source code formatter";
      };
    };

    # Network options
    net = {
      enable = lib.mkEnableOption "Enable network development & diagnostics tools";
    };

    # Nix options
    nix = {
      enable = lib.mkEnableOption "Enable Nix development tools";
    };

    # Nodejs options
    nodejs = {
      enable = lib.mkEnableOption "Enable JavaScript development tools";
      interpreter = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Enable the Node.js interpreter";
      };
    };

    # Python options
    python = {
      enable = lib.mkEnableOption "Enable Python development tools";
      interpreter = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Enable the Python interpreter";
      };
    };

    # GitHub options
    github = {
      enable = lib.mkEnableOption "Enable GitHub development tools";
    };

    # Rust options
    rust = {
      enable = lib.mkEnableOption "Enable Rust development tools";
    };

    # Shell options
    shell = {
      enable = lib.mkEnableOption "Enable Shell script development tools";
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

    home.packages = lib.mkMerge [
      # Docs
      (lib.mkIf cfg.docs.enable [
        pkgs.man-pages
        pkgs.man-pages-posix
      ])

      # C/C++
      (lib.mkIf cfg.cpp.enable [
        pkgs.ccls
      ])

      # Elm
      (lib.mkIf cfg.elm.enable [
        lib.elmPackages.elm-language-server
      ])

      # Haskell
      (lib.mkIf cfg.haskell.enable [
        (lib.mkIf cfg.haskell.lsp pkgs.haskell-language-server)
        (lib.mkIf cfg.haskell.ormolu pkgs.ormolu)
      ])

      # LLM
      (lib.mkIf cfg.llm.enable [
        (lib.mkIf cfg.llm.claude pkgs.claude-code)
        (lib.mkIf cfg.llm.copilot pkgs.github-copilot-cli)
        (lib.mkIf cfg.llm.gemini pkgs.gemini-cli)
      ])

      # Network
      (lib.mkIf cfg.net.enable [
        pkgs.dig
        pkgs.inetutils
        pkgs.mtr
        pkgs.nmap
        pkgs.wireshark
      ])

      # Nix
      (lib.mkIf cfg.nix.enable [
        pkgs.nil
      ])

      # Nodejs
      (lib.mkIf cfg.nodejs.enable [
        (lib.mkIf cfg.nodejs.interpreter pkgs.nodejs)
        pkgs.nodePackages.typescript
        pkgs.nodePackages.typescript-language-server
        pkgs.nodePackages.prettier
      ])

      # Python
      (lib.mkIf cfg.python.enable [
        (lib.mkIf cfg.python.interpreter (pkgs.python3.withPackages (ps: [
          ps.ipython
        ])))
        pkgs.black
        # pkgs.python-language-server
        pkgs.pyright
      ])

      # GitHub
      (lib.mkIf cfg.github.enable [
        pkgs.gitAndTools.gh
      ])

      # Rust
      (lib.mkIf cfg.rust.enable [
        pkgs.rust-analyzer
      ])

      # Shell
      (lib.mkIf cfg.shell.enable [
        pkgs.shellcheck
        pkgs.shfmt
      ])
    ];

    # Ghci configuration
    home.file = lib.mkMerge [
      (lib.mkIf cfg.haskell.enable {
        ".ghci".text = ''
          :set prompt "λ: "
        '';
      })
    ];

    # Neovim plugins
    programs.neovim.plugins = lib.mkIf cfg.haskell.enable [
      pkgs.vimPlugins.haskell-tools-nvim
    ];
  };
}
