-- vim.g.better_whitespace_filetypes_blacklist = {
--   'diff',
--   'gitcommit',
--   'qf',
--   'help',
--   'markdown',
--   'fugitive',
--   'toggleterm',
-- }
require('whitespace-nvim').setup{
  ignored_filetypes = {
    'TelescopePrompt',
    'Trouble',
    'toggleterm',
    'diff',
    'help',
    'markdown',
    'fugitive',

  }
}

vim.keymap.set('n', '<leader>w', require('whitespace-nvim').trim)
