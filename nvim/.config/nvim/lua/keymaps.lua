local opts = {silent = true}
-- set some basic normal mode keymaps, mostly using leader
vim.keymap.set('n', '<space>',    ':nohlsearch<CR>', opts)
vim.keymap.set('n', '<leader>b',  ':ls<CR>:b<space>', opts)
vim.keymap.set('n', '<leader>bd', ':bd<CR>', opts)


-- auto close curly brackets
vim.keymap.set('i', '{<CR>', '{<CR>}<ESC>O', opts)

-- insert blank lines, using '[' and ']' as leader
vim.keymap.set('n', ']<space>', [[:set paste<CR>m`o<ESC>``:set nopaste<CR>]], opts)
vim.keymap.set('n', '[<space>', [[:set paste<CR>m`O<ESC>``:set nopaste<CR>]], opts)

-- remove blank lines, using '[' and ']' as leader
vim.keymap.set('n', ']d', [[m`:silent +g/\m^\s*$/d<CR>``:noh<CR>]], opts)
vim.keymap.set('n', '[d', [[m`:silent -g/\m^s*$/d<CR>``:noh<CR>]], opts)

-- follow :h Terminal and bring back the escape key
vim.keymap.set('t', '<Esc>', '<C-\\><C-n>', opts)

-- consistent movement bindings
vim.keymap.set({'n', 't'}, '<C-h>', [[<Cmd>wincmd h<CR>]], opts)
vim.keymap.set({'n', 't'}, '<C-j>', [[<Cmd>wincmd j<CR>]], opts)
vim.keymap.set({'n', 't'}, '<C-k>', [[<Cmd>wincmd k<CR>]], opts)
vim.keymap.set({'n', 't'}, '<C-l>', [[<Cmd>wincmd l<CR>]], opts)
