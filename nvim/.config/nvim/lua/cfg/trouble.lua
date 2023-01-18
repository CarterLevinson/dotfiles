require('trouble').setup{}
--set up trouble keybindings

-- normal mode
nmap('<leader>x',  ':TroubleToggle<CR>')
nmap('<leader>xw', ':Trouble workspace_diagnostics<CR>')
nmap('<leader>xl', ':Trouble loclist<CR>')
nmap('<leader>xq', ':Trouble quickfix<CR>')
nmap('<leader>xr', ':Trouble lsp_references<CR>')
nmap('<leader>xc', ':TroubleClose<CR>')
-- vim.keymap.set('n', 'gR',         '<CMD>Trouble lsp_references<CR>', opts)
-- vim.keymap.set('n', '<leader>xt', '<CMD>TroubleToggle<CR>', opts

-- command aliases
vim.cmd[[cnoreabbrev copen Trouble quickfix]]
vim.cmd[[cnoreabbrev cclose TroubleClose]]
vim.cmd[[cnoreabbrev lopen Trouble loclist]]
vim.cmd[[cnoreabbrev lclose TroubleClose]]
