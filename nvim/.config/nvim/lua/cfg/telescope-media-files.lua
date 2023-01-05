require('telescope').load_extension('media_files')

local media_files = require('telescope').extensions.media_files
local opts = { noremap = true, silent = true }

-- \fmc: search through media files and copy its relative path to xclip
vim.keymap.set('n', '<leader>fmc', media_files.media_files, opts)
