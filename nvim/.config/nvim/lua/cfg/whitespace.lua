require('whitespace-nvim').setup{
  ignored_filetypes = {
    'TelescopePrompt',
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
Nmap('<leader>w', require('whitespace-nvim').trim)
