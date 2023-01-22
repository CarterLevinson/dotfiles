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

nmap('<leader>ws', whitespace.trim)
