require('trouble').setup{mode = 'document_diagnostics'}
--set up trouble keybindings
local opts = { silent = true, noremap = true }
-- normal mode
vim.keymap.set('n', '<leader>x',  ':TroubleToggle<CR>', opts)
vim.keymap.set('n', '<leader>xw', ':Trouble workspace_diagnostics<CR>', opts)
vim.keymap.set('n', '<leader>xl', ':Trouble loclist<CR>', opts)
vim.keymap.set('n', '<leader>xq', ':Trouble quickfix<CR>', opts)
vim.keymap.set('n', '<leader>xr', ':Trouble lsp_references<CR>', opts)
vim.keymap.set('n', '<leader>xc', ':TroubleClose<CR>', opts)
-- vim.keymap.set('n', 'gR',         '<CMD>Trouble lsp_references<CR>', opts)
-- vim.keymap.set('n', '<leader>xt', '<CMD>TroubleToggle<CR>', opts

-- command aliases
vim.cmd[[cnoreabbrev copen Trouble quickfix]]
vim.cmd[[cnoreabbrev cclose TroubleClose]]
vim.cmd[[cnoreabbrev lopen Trouble loclist]]
vim.cmd[[cnoreabbrev lclose TroubleClose]]
