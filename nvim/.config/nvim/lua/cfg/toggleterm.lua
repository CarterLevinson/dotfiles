require('toggleterm').setup{
  -- direction = 'float',
  open_mapping = '<leader>t',

  insert_mapping = true,
  terminal_mapping = true,

  persist_mode = true,
  close_on_exit = true,


  shade_terminals = true,
  float_opts = {
    border = 'curved',
  },
}

-- custom user commands based off toggle term functionality
vim.cmd[[com! -nargs=1 Cppman
\ :10TermEx direction=horizontal size=50 go_back=0 cmd='cppman <args>'<CR>
]]

-- vim.cmd[[com! -nargs=1 Hoogle
-- \ :11TermEx direction=horizontal size=50 go_back=0 cmd='hoogle <args>'<CR>
-- ]]

vim.cmd[[com! -nargs=? Hman
\ :12TermEx direction=horizontal size=50 go_back=0 cmd='hdc <args>'<CR>
]]

vim.cmd[[com! -nargs=1 Pyman
\ :13TermEx direction=horizontal size=50 go_back=0 cmd='pydoc <args> | less'<CR>
]]

vim.cmd[[com! -nargs=1 Glow
\ :14TermEx direction=horizontal size=50 go_back=0 cmd='glow -p <args>'<CR>
]]

local opts = {silent = true, noremap = true}
vim.keymap.set('n', '<leader>tt', '<CMD>ToggleTermSendCurrentLine<CR>', opts)
vim.keymap.set('n', '<leader>tv', '<CMD>ToggleTermSendVisualSelection<CR>',opts)
vim.keymap.set('n', '<leader>tV', '<CMD>ToggleTermSendVisualLines<CR>', opts)
vim.keymap.set('n', '<leader>ta', '<CMD>ToggleTermToggleAll<CR>', opts)
vim.keymap.set('t', '<leader>tc', '<CMD>close<CR>', opts)
