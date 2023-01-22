local setlocal       = vim.opt_local
setlocal.tabstop     = 4
setlocal.shiftwidth  = 4
setlocal.softtabstop = 4
setlocal.equalprg    = 'yapf'

-- custom user commands based off toggle term functionality
vim.api.nvim_buf_create_user_command(0, "Pydoc",
  function(opts)
    vim.cmd [[new]]
    vim.cmd([[r ! pydoc ]] .. opts.args)
    vim.cmd [[1]]
  end,
  { nargs = 1, desc = "Browse python docs using pydoc" }
)
