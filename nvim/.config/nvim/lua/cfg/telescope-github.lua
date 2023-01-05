require('telescope').load_extension('gh')

local github = require('telescope').extensions.gh
local opts = { noremap = true, silent = true }

-- \fpr: search through github pull requests
vim.keymap.set('n', '<leader>fpr', github.pull_request, opts)
-- \fgi: search through github issue list
vim.keymap.set('n', '<leader>fgi', github.issues, opts)
-- \fgg: search through github gists
vim.keymap.set('n', '<leader>fgg', github.gist, opts)
