local haskell_tools  = require('haskell_tools')
local setlocal       = vim.bo

setlocal.tabstop     = 4
setlocal.shiftwidth  = 4
setlocal.softtabstop = 4
setlocal.formatprg   = 'stylish-haskell'
setlocal.equalprg    = 'hindent'


local function bufferGHCi()
  haskell_tools.repl.toggle(vim.api.nvim_buf_get_name(0))
end

-- haskell tools keybindings
local opts = {buffer = true}

-- Toggle a GHCi repl for the current package
vim.keymap.set('n', '<leader>rr', haskell_tools.repl.toggle, opts)
-- Toggle a GHCi repl for the current buffer
vim.keymap.set('n', '<leader>rf', bufferGHCi, opts)
-- close a GHCi repl
vim.keymap.set('n', '<leader>rq', haskell_tools.repl.quit, opts)

-- haskell tools specific lsp
vim.keymap.set('n', '<space>cl',  vim.lsp.codelens.run, opts)
vim.keymap.set('n', '<leader>hs', haskell_tools.hoogle.hoogle_signature, opts)
