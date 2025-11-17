---
-- Completions
---

local cmp = require('cmp')
local cmp_select = {behavior = cmp.SelectBehavior.Select}

-- Load snippets from VSCode extensions (friendly snippets, etc.)
require("luasnip.loaders.from_vscode").lazy_load()


cmp.setup({
  sources = {
    {name = 'path'},
    {name = 'nvim_lsp'},
    {name = 'nvim_lua'},
    {name = 'luasnip', keyword_length = 2},
    {name = 'buffer', keyword_length = 3},
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
    ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
    ['<C-y>'] = cmp.mapping.confirm({ select = true }),
    ['<C-Space>'] = cmp.mapping.complete(),
  }),
})

-- local sl = require("luasnip.extras.snippet_list")
-- local function printer(snippets)
--     local res = ""
--
--     for ft, snips in pairs(snippets) do
--         res = res .. ft .. "\n"
--         for _, snip in pairs(snips) do
--             res = res .. "    " .. "Name: " .. snip.name .. "\n"
--             res = res .. "    " .. "Desc: " .. snip.description[1] .. "\n"
--             res = res .. "    " .. "Trigger: " .. snip.trigger .. "\n"
--             res = res .. "    ----" .. "\n"
--         end
--     end
--
--     return res
-- end
--
--
-- -- using it
-- sl.open({printer = printer})
