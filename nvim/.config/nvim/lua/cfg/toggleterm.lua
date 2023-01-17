require('utils.map')
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

-- keymaps
nmap('<leader>tt', '<CMD>ToggleTermSendCurrentLine<CR>')
nmap('<leader>tv', '<CMD>ToggleTermSendVisualSelection<CR>')
nmap('<leader>tV', '<CMD>ToggleTermSendVisualLines<CR>')
nmap('<leader>ta', '<CMD>ToggleTermToggleAll<CR>')
tmap('<leader>tc', '<CMD>close<CR>')
