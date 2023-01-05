require('telescope').load_extension('hoogle')

local hoogle = require('telescope').extensions.hoogle
local opts =  { noremap = true, silent = true }

-- \fh: search through hoogle
vim.keymap.set('n', '<leader>fh', hoogle.hoogle, opts)
