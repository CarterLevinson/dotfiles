local telescope = require('telescope')
telescope.load_extension('hoogle')
local hoogle = telescope.extensions.hoogle

-- \fh: search through hoogle
vim.keymap.set('n', '<leader>fh', hoogle.hoogle)
