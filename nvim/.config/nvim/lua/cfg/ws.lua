local whitespace = require("whitespace-nvim")
whitespace.setup{
  ignored_filetypes = {
    'TelescopePrompt',
    'fzf',
    'FzfLua',
    'FZF',
    'Trouble',
    'toggleterm',
    'diff',
    'help',
    'markdown',
    'fugitive',
    'lspinfo',
    'lazy',
  }
}

-- nmap('<leader>s', whitespace.trim)

vim.api.nvim_create_user_command("Trim",
  whitespace.trim,
  { desc = "Strip all whitespace in buffer" }
)
