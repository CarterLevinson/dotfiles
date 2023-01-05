-- set some basic normal mode keymaps, mostly using leader
vim.keymap.set('n', '<space>',    ':nohlsearch<CR>')
vim.keymap.set('n', '<leader>w',  ':w<CR>')
vim.keymap.set('n', '<leader>q',  ':q!<CR>')
vim.keymap.set('n', '<leader>b',  ':ls<CR>:b<space>')
vim.keymap.set('n', '<leader>ls', ':ls<CR>')
vim.keymap.set('n', '<leader>bd', ':bd<CR>')
vim.keymap.set('n', '<leader>bn', ':bn<CR>')
vim.keymap.set('n', '<leader>bp', ':bp<CR>')
--vim.keymap.set('n', '<leader>cl',  ':close<CR>')

local opts = { silent = true, noremap = true }

-- auto close curly brackets
vim.keymap.set('i', '{<CR>', '{<CR>}<ESC>O', opts)

-- insert blank lines, using '[' and ']' as leader
vim.keymap.set('n', ']<space>', [[:set paste<CR>m`o<ESC>``:set nopaste<CR>]], opts)
vim.keymap.set('n', '[<space>', [[:set paste<CR>m`O<ESC>``:set nopaste<CR>]], opts)

-- remove blank lines, using '[' and ']' as leader
vim.keymap.set('n', ']d', [[m`:silent +g/\m^\s*$/d<CR>``:noh<CR>]], opts)
vim.keymap.set('n', '[d', [[m`:silent -g/\m^s*$/d<CR>``:noh<CR>]], opts)

-- follow :h Terminal and bring back the escape key
vim.keymap.set('t', '<Esc>', '<C-\\><C-n>')

-- consistent movement bindings
vim.keymap.set('t', '<C-h>', [[<Cmd>wincmd h<CR>]])
vim.keymap.set('t', '<C-j>', [[<Cmd>wincmd j<CR>]])
vim.keymap.set('t', '<C-k>', [[<Cmd>wincmd k<CR>]])
vim.keymap.set('t', '<C-l>', [[<Cmd>wincmd l<CR>]])

