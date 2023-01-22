local setlocal = vim.opt_local
setlocal.cinoptions = vim.bo.cinoptions .. "L0"

-- custom user commands based off toggle term functionality
vim.api.nvim_buf_create_user_command(0, "Cppman",
  function(opts)
    vim.cmd [[new]]
    vim.cmd([[r ! cppman ]] .. opts.args)
    vim.cmd [[Man!]]
    vim.cmd [[1]]
  end,
  { nargs = 1 }
)
