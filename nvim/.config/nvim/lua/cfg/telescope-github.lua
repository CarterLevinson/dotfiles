local telescope = require('telescope')
telescope.load_extension('gh')
local github = telescope.extensions.gh

-- \fpr: search through github pull requests
vim.keymap.set('n', '<leader>fpr', github.pull_request)
-- \fgi: search through github issue list
vim.keymap.set('n', '<leader>fgi', github.issues)
-- \fgg: search through github gists
vim.keymap.set('n', '<leader>fgg', github.gist)
