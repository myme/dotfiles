# TODO: Pair programming setup using Docker/Podman + Tmate
# Use a docker/pod with tmate and usual dev tools to share a tmux session with a peer.

{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.myme.dev;

  # Claude Code sessions grow to several GB each when left running for days,
  # and a handful of them is enough to push the machine into a global OOM. The
  # kernel then picks victims by oom_score_adj rather than size, and has been
  # known to pick the user's systemd manager -- taking the whole graphical
  # session and tmux down with it. So each session runs in its own scope
  # under `llm.slice`, capped individually and as a group, and volunteers
  # itself as the first thing to kill. A lost session is cheap
  # (`claude --resume`); a lost desktop is not.
  claudeScope = pkgs.writeShellScript "claude" ''
    # Raising one's own score needs no privileges, and works without systemd.
    echo 500 > /proc/$$/oom_score_adj 2>/dev/null || true

    # Not when already scoped (a `claude` spawned from a Claude session), nor
    # without a user manager to ask (WSL without systemd, bare containers).
    if [ -z "''${MYME_CLAUDE_SCOPE:-}" ] && [ -S "''${XDG_RUNTIME_DIR:-}/bus" ] \
      && command -v systemd-run >/dev/null; then
      export MYME_CLAUDE_SCOPE=1
      exec systemd-run --user --scope --quiet --collect \
        --slice=llm.slice --unit="claude-$$" \
        --description="Claude Code in $PWD" \
        -p MemoryHigh=4G -p MemoryMax=6G \
        ${lib.getExe pkgs.claude-code} "$@"
    fi
    exec ${lib.getExe pkgs.claude-code} "$@"
  '';

  claude =
    if pkgs.stdenv.hostPlatform.isLinux then
      pkgs.symlinkJoin {
        name = "claude-code-scoped-${pkgs.claude-code.version}";
        paths = [ pkgs.claude-code ];
        postBuild = ''
          rm $out/bin/claude
          ln -s ${claudeScope} $out/bin/claude
        '';
        meta.mainProgram = "claude";
      }
    else
      pkgs.claude-code;

in
{
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

    # LLM support (Claude, Codex, Copilot, ...)
    llm = {
      enable = lib.mkEnableOption "Enable LLM editor integrations";
      claude = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Enable the Claude Code CLI tool";
        };
      };
      codex = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable the Codex CLI tool";
      };
      copilot = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable the Copilot CLI tool";
      };
      ollama = {
        enable = lib.mkEnableOption "Enable the Ollama LLM runner";
        package = lib.mkOption {
          type = lib.types.package;
          default = pkgs.ollama;
          defaultText = lib.literalExpression "pkgs.ollama";
          description = ''
            Which Ollama build to install. The default is CPU-only: the
            accelerated backends have to be compiled in, so a machine whose
            host can actually reach a GPU wants `pkgs.ollama-cuda` or
            `pkgs.ollama-rocm` here.
          '';
        };
      };
      opencode.enable = lib.mkEnableOption "Enable opencode";
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
        inherit (cfg.docs) enable;
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
        (lib.mkIf cfg.llm.claude.enable claude)
        (lib.mkIf cfg.llm.codex pkgs.codex)
        (lib.mkIf cfg.llm.copilot pkgs.github-copilot-cli)
        (lib.mkIf cfg.llm.ollama.enable cfg.llm.ollama.package)
        (lib.mkIf cfg.llm.opencode.enable pkgs.opencode)
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
        pkgs.typescript
        pkgs.typescript-language-server
        pkgs.prettier
      ])

      # Python
      (lib.mkIf cfg.python.enable [
        (lib.mkIf cfg.python.interpreter (
          pkgs.python3.withPackages (ps: [
            ps.ipython
          ])
        ))
        pkgs.black
        # pkgs.python-language-server
        pkgs.pyright
      ])

      # GitHub
      (lib.mkIf cfg.github.enable [
        pkgs.gh
        pkgs.gh-stack
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
      (lib.mkIf (cfg.llm.enable && cfg.llm.claude.enable) {
        ".claude/settings.json".text = builtins.toJSON {
          includeCoAuthoredBy = false;
        };
      })
    ];

    # Shared budget for all Claude sessions (see `claudeScope` above). Above
    # MemoryHigh the slice is reclaimed into swap; at MemoryMax a session in
    # it gets OOM-killed, rather than something elsewhere on the machine.
    systemd.user.slices.llm =
      lib.mkIf (cfg.llm.enable && cfg.llm.claude.enable && pkgs.stdenv.hostPlatform.isLinux)
        {
          Unit.Description = "LLM agents";
          Slice = {
            MemoryHigh = "12G";
            MemoryMax = "16G";
          };
        };

    # Neovim plugins
    programs.neovim.plugins = lib.mkIf cfg.haskell.enable [
      pkgs.vimPlugins.haskell-tools-nvim
    ];
  };
}
