---
-- Enable LSP servers
---

local capabilities = require('cmp_nvim_lsp').default_capabilities()

vim.lsp.config('denols', {
  capabilities = capabilities,
})
vim.lsp.enable('denols')

vim.lsp.config('eslint', {
  capabilities = capabilities,
})
vim.lsp.enable('eslint')

vim.lsp.config('html', {
  capabilities = capabilities,
})
vim.lsp.enable('pyright')

vim.lsp.config('gopls', {
  capabilities = capabilities,
})
vim.lsp.enable('gopls')

vim.lsp.config('ts_ls', {
  capabilities = capabilities,
  root_dir = function(bufnr, on_dir)
    -- Don't start ts_ls if we're in a Deno project
    local filename = vim.api.nvim_buf_get_name(bufnr)
    local deno_markers = vim.fs.find({ 'deno.json', 'deno.jsonc' }, {
      upward = true,
      path = vim.fs.dirname(filename),
    })
    if #deno_markers > 0 then
      return
    end
    -- Otherwise find root using package.json or tsconfig.json
    local root = vim.fs.root(bufnr, { 'package.json', 'tsconfig.json' })
    on_dir(root)
  end,
})
vim.lsp.enable('ts_ls')
