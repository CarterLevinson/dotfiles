local setlocal       = vim.bo
setlocal.tabstop     = 4
setlocal.shiftwidth  = 4
setlocal.softtabstop = 4
setlocal.formatprg   = 'yapf'

-- custom user commands based off toggle term functionality
vim.api.nvim_create_user_command(
  "Pydoc",
  function(opts)
    vim.cmd[[new]]
    vim.cmd([[r ! pydoc ]] .. opts.args)
    vim.cmd[[1]]
  end,
  {nargs = 1}
)
