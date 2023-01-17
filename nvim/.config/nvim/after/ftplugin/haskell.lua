local haskell_tools = require("haskell-tools")
local map           = require("utils.map")

local function bufferGHCi()
  haskell_tools.repl.toggle(vim.api.nvim_buf_get_name(0))
end

local setlocal       = vim.bo

setlocal.tabstop     = 4
setlocal.shiftwidth  = 4
setlocal.softtabstop = 4
setlocal.formatprg   = "stylish-haskell"
setlocal.equalprg    = "hindent"

-- custom user commands based off toggle term functionality
-- vim.cmd[[com! -nargs=1 Hoogle
-- \ :11TermEx direction=horizontal size=50 go_back=0 cmd='hoogle <args>'<CR>
-- ]]

-- vim.cmd[[com! -nargs=? Hman
-- \ :12TermEx direction=horizontal size=50 go_back=0 cmd='hdc <args>'<CR>
-- ]]

-- haskell tools keybindings

-- hlint refactor plugin
-- bindings:
-- to apply one hint at cursor pos
-- ta apply all suggestions in the file
local opts = {buffer = 0}

-- Toggle a GHCi repl for the current package
nmap('<leader>rr', haskell_tools.repl.toggle, opts)
-- Toggle a GHCi repl for the current buffer
nmap('<leader>rf', bufferGHCi, opts)
-- close a GHCi repl
nmap('<leader>rq', haskell_tools.repl.quit, opts)

-- haskell tools specific lsp
nmap('<space>cl',  vim.lsp.codelens.run, opts)
nmap('<leader>hs', haskell_tools.hoogle.hoogle_signature, opts)

-- haskell point free plugin
nxmap('<leader>pf', '<Plug>Pointfree', opts)

