---
-- Testing
---

require('neotest').setup({
  adapters = {
    require("neotest-haskell"),
    require("neotest-python"),
    require("rustaceanvim.neotest"),
  }
})
