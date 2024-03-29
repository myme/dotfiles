{ pkgs, ... }: {
  config = {
    programs.neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;

      plugins = with pkgs.vimPlugins; [
        # Completions + LSP (lsp-zero)
        copilot-vim
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

        " Neotree
        nnoremap <leader>op <cmd>Neotree toggle<cr>

        " Telescope
        nnoremap <leader>. <cmd>Telescope find_files<cr>
        nnoremap <leader>, <cmd>Telescope buffers<cr>
        nnoremap <leader>g. <cmd>Telescope git_files<cr>
        nnoremap <leader>sd <cmd>Telescope live_grep<cr>
        nnoremap <leader>sb <cmd>Telescope current_buffer_fuzzy_find<cr>
        nnoremap <leader>sp <cmd>Telescope grep_string<cr>
        nnoremap <leader>hW <cmd>Telescope man_pages<cr>

        " Themes
        set termguicolors
        colorscheme dracula
        let g:airline_theme='base16_dracula'

        " Filetype plugins
        filetype plugin on
        filetype indent on
      '';

      extraLuaConfig = ''
        require('dashboard').setup({})

        local lsp_zero = require('lsp-zero').preset({})

        lsp_zero.on_attach(function(client, bufnr)
          lsp_zero.default_keymaps({buffer = bufnr})
        end)

        -- When you don't have mason.nvim installed
        -- You'll need to list the servers installed in your system
        lsp_zero.setup_servers({'tsserver', 'eslint'})

        lsp_zero.setup()

        ---
        -- Setup haskell LSP
        ---

        vim.g.haskell_tools = {
          hls = {
            capabilities = lsp_zero.get_capabilities()
          }
        }

        -- Autocmd that will actually be in charging of starting hls
        local hls_augroup = vim.api.nvim_create_augroup('haskell-lsp', {clear = true})
        vim.api.nvim_create_autocmd('FileType', {
          group = hls_augroup,
          pattern = {'haskell'},
          callback = function()
            ---
            -- Suggested keymaps from the quick setup section:
            -- https://github.com/mrcjkb/haskell-tools.nvim#quick-setup
            ---

            local ht = require('haskell-tools')
            local bufnr = vim.api.nvim_get_current_buf()
            local def_opts = { noremap = true, silent = true, buffer = bufnr, }
            -- haskell-language-server relies heavily on codeLenses,
            -- so auto-refresh (see advanced configuration) is enabled by default
            vim.keymap.set('n', '<space>ca', vim.lsp.codelens.run, opts)
            -- Hoogle search for the type signature of the definition under the cursor
            vim.keymap.set('n', '<space>hs', ht.hoogle.hoogle_signature, opts)
            -- Evaluate all code snippets
            vim.keymap.set('n', '<space>ea', ht.lsp.buf_eval_all, opts)
            -- Toggle a GHCi repl for the current package
            vim.keymap.set('n', '<leader>rr', ht.repl.toggle, opts)
            -- Toggle a GHCi repl for the current buffer
            vim.keymap.set('n', '<leader>rf', function()
              ht.repl.toggle(vim.api.nvim_buf_get_name(0))
            end, def_opts)
            vim.keymap.set('n', '<leader>rq', ht.repl.quit, opts)
          end
        })

        -- Completions
        local cmp = require('cmp')
        local cmp_select = {behavior = cmp.SelectBehavior.Select}


        cmp.setup({
          sources = {
            {name = 'path'},
            {name = 'nvim_lsp'},
            {name = 'nvim_lua'},
            {name = 'luasnip', keyword_length = 2},
            {name = 'buffer', keyword_length = 3},
          },
          formatting = lsp_zero.cmp_format(),
          mapping = cmp.mapping.preset.insert({
            ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
            ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
            ['<C-y>'] = cmp.mapping.confirm({ select = true }),
            ['<C-Space>'] = cmp.mapping.complete(),
          }),
        })
      '';
    };
  };
}
