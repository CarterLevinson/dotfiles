local haskell_tools  = require('haskell_tools')

local function bufferGHCi()
  haskell_tools.repl.toggle(vim.api.nvim_buf_get_name(0))
end

local setlocal       = vim.bo

setlocal.tabstop     = 4
setlocal.shiftwidth  = 4
setlocal.softtabstop = 4
setlocal.formatprg   = 'stylish-haskell'
setlocal.equalprg    = 'hindent'

-- custom user commands based off toggle term functionality
vim.cmd[[com! -nargs=1 Hoogle
\ :11TermEx direction=horizontal size=50 go_back=0 cmd='hoogle <args>'<CR>
]]

vim.cmd[[com! -nargs=? Hman
\ :12TermEx direction=horizontal size=50 go_back=0 cmd='hdc <args>'<CR>
]]

-- haskell tools keybindings
local opts = {buffer = 0}

-- hlint refactor plugin
-- bindings:
-- to apply one hint at cursor pos
-- ta apply all suggestions in the file

-- Toggle a GHCi repl for the current package
vim.keymap.set('n', '<leader>rr', haskell_tools.repl.toggle, opts)
-- Toggle a GHCi repl for the current buffer
vim.keymap.set('n', '<leader>rf', bufferGHCi, opts)
-- close a GHCi repl
vim.keymap.set('n', '<leader>rq', haskell_tools.repl.quit, opts)

-- haskell tools specific lsp
vim.keymap.set('n', '<space>cl',  vim.lsp.codelens.run, opts)
vim.keymap.set('n', '<leader>hs', haskell_tools.hoogle.hoogle_signature, opts)

-- haskell point free plugin
vim.keymap.set({'n', 'x'}, '<leader>pf', '<Plug>Pointfree')

