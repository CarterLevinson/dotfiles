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

vim.api.nvim_create_autocmd("BufWritePre", { callback = whitespace.trim })
