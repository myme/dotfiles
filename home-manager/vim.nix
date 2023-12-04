{ pkgs, ... }: {
  config = {
    programs.neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;

      plugins = with pkgs.vimPlugins; [
        # Completions + LSP (lsp-zero)
        lsp-zero-nvim
        nvim-cmp
        cmp-nvim-lsp
        nvim-lspconfig
        luasnip

        # Git
        gitv
        vim-fugitive

        # Telescope (fuzzy)
        telescope-fzf-native-nvim
        telescope-nvim

        # Theme
        dracula-nvim
        rose-pine
        vim-airline
        vim-airline-themes

        # Languages
        vim-nix

        # Tpope
        vim-sensible
        vim-surround
        vim-unimpaired
        vim-vinegar

        # Utils
        # vim-which-key
      ];

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

        " Telescope
        nnoremap <leader>. <cmd>Telescope find_files<cr>
        nnoremap <leader>, <cmd>Telescope buffers<cr>
        nnoremap <leader>g. <cmd>Telescope git_files<cr>
        nnoremap <leader>sd <cmd>Telescope live_grep<cr>
        nnoremap <leader>sb <cmd>Telescope current_buffer_fuzzy_find<cr>
        nnoremap <leader>sp <cmd>Telescope grep_string<cr>
        nnoremap <leader>hW <cmd>Telescope man_pages<cr>

        " Themes
        colorscheme dracula
        let g:airline_theme='base16_dracula'
      '';

      extraLuaConfig = ''

        local lsp = require('lsp-zero').preset({})

        lsp.on_attach(function(client, bufnr)
          lsp.default_keymaps({buffer = bufnr})
        end)

        -- When you don't have mason.nvim installed
        -- You'll need to list the servers installed in your system
        lsp.setup_servers({'tsserver', 'eslint'})

        lsp.setup()
      '';
    };
  };
}
