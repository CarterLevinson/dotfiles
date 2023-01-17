require('utils.map')
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
  }
}
nmap('<leader>w', require('whitespace-nvim').trim)
