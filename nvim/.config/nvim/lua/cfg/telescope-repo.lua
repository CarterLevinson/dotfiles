local telescope = require('telescope')
telescope.load_extension('repo')
local repo = telescope.extensions.repo

-- \fgr: search through git repos on system
vim.keymap.set('n', '<leader>fgr', repo.repo)
-- \lgr: list git repos available on system
vim.keymap.set('n', '<leader>lgr', repo.list)
