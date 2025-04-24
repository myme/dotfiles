{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.myme.vim;

in
{
  options.myme.vim = {
    enable = lib.mkEnableOption "(Neo)Vim";
    default-editor = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Set vim as $EDITOR";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.vscode-langservers-extracted
    ];

    home.sessionVariables = lib.mkIf cfg.default-editor {
      EDITOR = "vim";
    };

    programs.neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;

      plugins =
        with pkgs.vimPlugins;
        (lib.mkMerge [
          # LLM coding tools
          (lib.mkIf config.myme.dev.llm.enable [
            {
              plugin = avante-nvim;
              type = "lua";
              config = ''
                require("avante_lib").load()
                require("avante").setup()
              '';
            }
            copilot-vim
          ])
          [
            # Completions + LSP (lsp-zero)
            lsp-zero-nvim
            nvim-cmp
            cmp-nvim-lsp
            nvim-lspconfig
            luasnip

            # Dev
            neotest
            neotest-haskell
            neotest-python
            neotest-rust
            nvim-dap
            nvim-dap-ui

            # UI
            dashboard-nvim
            neo-tree-nvim
            nvim-web-devicons

            # Git
            gitv
            vim-fugitive

            # Telescope (fuzzy)
            telescope-fzf-native-nvim
            telescope-nvim

            # Theme
            catppuccin-nvim
            dracula-nvim
            rose-pine
            vim-airline
            vim-airline-themes

            # Languages
            go-nvim
            haskell-tools-nvim
            vim-nix
            rustaceanvim
            nvim-treesitter

            # Tpope
            vim-sensible
            vim-surround
            vim-unimpaired
            vim-vinegar

            # Utils
            which-key-nvim
          ]
        ]);

      extraConfig = ''
        let mapleader=" "

        " Standard options
        set   cmdheight=1
        set nocursorcolumn
        set nocursorline
        set   laststatus=2
        set   modeline
        set   relativenumber
        set   numberwidth=1
        set   scrolloff=0
        set   showcmd

        " Folds
        set nofoldenable
        set   foldlevel=1
        set   foldmethod=syntax

        " Whitespace
        set   expandtab
        set   shiftwidth=4
        set   tabstop=4
        set nowrap

        " Tab completion
        set   wildchar=<tab>
        set   wildmode=longest:full,full

        " Backup + Swap
        set nobackup
        set noswapfile

        " Git / fugitive
        nmap <silent> <Leader>gg :Git<Return>

        " Neotree
        nnoremap <leader>op <cmd>Neotree toggle<cr>

        " Telescope
        nnoremap <leader>. <cmd>Telescope find_files<cr>
        nnoremap <leader>, <cmd>Telescope buffers<cr>
        nnoremap <leader>g. <cmd>Telescope git_files<cr>
        nnoremap <leader>s" <cmd>Telescope registers<cr>
        nnoremap <leader>sd <cmd>Telescope live_grep<cr>
        nnoremap <leader>sb <cmd>Telescope current_buffer_fuzzy_find<cr>
        nnoremap <leader>si <cmd>Telescope lsp_workspace_symbols<cr>
        nnoremap <leader>sk <cmd>Telescope keymaps<cr>
        nnoremap <leader>sp <cmd>Telescope grep_string<cr>
        nnoremap <leader>hW <cmd>Telescope man_pages<cr>

        " Themes
        set termguicolors
        colorscheme dracula
        let g:airline_theme='base16_dracula'

        " Filetype plugins
        filetype plugin on
        filetype indent on

        " Lua configs
        luafile ${./authinfo.lua}
        luafile ${./config.lua}
        luafile ${./lsp.lua}
        luafile ${./testing.lua}
        luafile ${./completions.lua}
      '';
    };
  };
}
