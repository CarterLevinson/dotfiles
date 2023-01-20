local haskell_tools = require("haskell-tools")

local function bufferGHCi()
  haskell_tools.repl.toggle(vim.api.nvim_buf_get_name(0))
end

local opts = {buffer = 0}
-- hlint refactor plugin
-- bindings:
-- `to` apply one hint at cursor pos
-- `ta` apply all suggestions in the file

-- Toggle a GHCi repl for the current package
Nmap("<leader>rr", haskell_tools.repl.toggle, opts)
-- Toggle a GHCi repl for the current buffer
Nmap("<leader>rf", bufferGHCi, opts)
-- close a GHCi repl
Nmap("<leader>rq", haskell_tools.repl.quit, opts)

-- haskell tools specific lsp
Nmap("<space>cl",  vim.lsp.codelens.run, opts)
Nmap("<leader>ho", haskell_tools.hoogle.hoogle_signature, opts)

-- haskell point free plugin
NXmap("<leader>pf", "<Plug>Pointfree", opts)

local setlocal       = vim.bo

setlocal.tabstop     = 4
setlocal.shiftwidth  = 4
setlocal.softtabstop = 4
setlocal.equalprg    = "hindent"

vim.api.nvim_create_user_command("HDC",
  vim.cmd[[TermEx direction=horizontal go_back = 0, cmd = 'hdc']]
)
