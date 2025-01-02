---
-- LSP Zero
---

local lsp_zero = require('lsp-zero').preset({})

lsp_zero.on_attach(function(client, bufnr)
  lsp_zero.default_keymaps({
      buffer = bufnr,
      preserve_mappings = false
  })
  vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, opts)
  vim.keymap.set('n', '<space>cf', vim.lsp.buf.format, opts)
  vim.keymap.set('n', '<space>cr', vim.lsp.buf.rename, opts)
  vim.keymap.set('n', '<space>cj', require('telescope.builtin').lsp_workspace_symbols, opts)
  vim.keymap.set('n', '<space>cs', require('telescope.builtin').lsp_document_symbols, opts)
end)

vim.g.rustaceanvim = function()
  return {
    server = {
      on_attach = function (client,bufnr)
          lsp_zero.default_keymaps({
              buffer = bufnr,
              preserve_mappings = false
          })
      end
    }
  }
end

-- When you don't have mason.nvim installed
-- You'll need to list the servers installed in your system
lsp_zero.setup_servers({'ts_ls', 'eslint'})
lsp_zero.setup()

lspconfig = require('lspconfig')

---
-- Setup Pyright LSP
---

lspconfig.pyright.setup{}

---
-- Setup Go nvim
---

require('go').setup()
lspconfig.gopls.setup{}

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
