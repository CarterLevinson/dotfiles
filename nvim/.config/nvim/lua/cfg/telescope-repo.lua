require('telescope').load_extension('repo')

local repo = require('telescope').extensions.repo
local opts = { noremap = true, silent = true }

-- \fgr: search through git repos on system
vim.keymap.set('n', '<leader>fgr', repo.repo, opts)
-- \lgr: list git repos available on system
vim.keymap.set('n', '<leader>lgr', repo.list, opts)
