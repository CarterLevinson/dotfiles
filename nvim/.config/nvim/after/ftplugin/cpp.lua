local setlocal = vim.bo
setlocal.cinoptions = vim.bo.cinoptions .. "L0"
setlocal.formatprg = "clang-format"

-- custom user commands based off toggle term functionality
vim.api.nvim_create_user_command(
  "Cppman",
  function(opts)
    vim.cmd[[new]]
    vim.cmd([[r ! cppman ]] .. opts.args)
    vim.cmd[[Man!]]
    vim.cmd[[1]]
  end,
  {nargs = 1}
)

