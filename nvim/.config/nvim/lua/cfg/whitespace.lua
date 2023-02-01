local whitespace = require("whitespace-nvim")

whitespace.setup {
  ignored_filetypes = {
    'TelescopePrompt',
    'fzf',
    'Trouble',
    'toggleterm',
    'diff',
    'help',
    'markdown',
    'fugitive',
    'lspinfo',
    'lazy',
    'quickfix',
  }
}

vim.api.nvim_create_autocmd("BufWritePre", { callback = whitespace.trim })
