require('trouble').setup{}
--set up trouble keybindings

-- normal mode
vim.keymap.set('n', '<leader>x',  ':TroubleToggle<CR>')
vim.keymap.set('n', '<leader>xw', ':Trouble workspace_diagnostics<CR>')
vim.keymap.set('n', '<leader>xl', ':Trouble loclist<CR>')
vim.keymap.set('n', '<leader>xq', ':Trouble quickfix<CR>')
vim.keymap.set('n', '<leader>xr', ':Trouble lsp_references<CR>')
vim.keymap.set('n', '<leader>xc', ':TroubleClose<CR>')
-- vim.keymap.set('n', 'gR',         '<CMD>Trouble lsp_references<CR>', opts)
-- vim.keymap.set('n', '<leader>xt', '<CMD>TroubleToggle<CR>', opts

-- command aliases
vim.cmd[[cnoreabbrev copen Trouble quickfix]]
vim.cmd[[cnoreabbrev cclose TroubleClose]]
vim.cmd[[cnoreabbrev lopen Trouble loclist]]
vim.cmd[[cnoreabbrev lclose TroubleClose]]
