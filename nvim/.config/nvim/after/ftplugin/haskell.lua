local setlocal       = vim.opt_local
setlocal.tabstop     = 4
setlocal.shiftwidth  = 4
setlocal.softtabstop = 4
setlocal.equalprg    = "hindent"

-- haskell point free plugin
nxmap("<leader>pf", "<Plug>Pointfree", { buffer = 0 })

vim.api.nvim_buf_create_user_command(0, "Hdc",
  function(_) vim.cmd [[TermEx direction=float go_back=0 cmd="hdc"]] end,
  { desc = "Browse Haskell docs using hdc and toggleterm" }
)

vim.api.nvim_buf_create_user_command(0, "GHCiToggle",
  function(_) require "haskell-tools".repl.toggle() end,
  { desc = "Toggle GHCi repl for current package" }
)

vim.api.nvim_buf_create_user_command(0, "GHCiToggleBuffer",
  function(_) require "haskell-tools".repl.toggle(vim.fn.expand("%")) end,
  { desc = "Toggle GHCi repl for current buffer" }
)

vim.api.nvim_buf_create_user_command(0, "GHCiClose",
  function(_) require "haskell-tools".repl.quit() end,
  { desc = "Close GHCi repl window" }
)
