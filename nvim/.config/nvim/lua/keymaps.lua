local opts = {silent = true}
-- set some basic normal mode keymaps, mostly using leader
vim.keymap.set('n', '<space>',    ':nohlsearch<CR>', opts)
vim.keymap.set('n', '<leader>b',  ':ls<CR>:b<space>', opts)
vim.keymap.set('n', '<leader>bd', ':bd<CR>', opts)
vim.keymap.set('n', '<leader>s', ':w<CR>', opts)

-- pseudo auto close
vim.keymap.set('i', '{<CR>', '{<CR>}<ESC>O', opts)
-- vim.keymap.set('i', '<C-a>', '<ESC>a', opts)
-- vim.keymap.set('i', [[']], [['<ESC>ylpi]], opts)
-- vim.keymap.set('i', [["]], [["<ESC>ylpi]], opts)

-- follow :h Terminal and bring back the escape key
vim.keymap.set('t', '<Esc>', '<C-\\><C-n>', opts)
-- better movement bindings
vim.keymap.set({'n', 't'}, '<C-h>', [[<CMD>wincmd h<CR>]], opts)
vim.keymap.set({'n', 't'}, '<C-j>', [[<CMD>wincmd j<CR>]], opts)
vim.keymap.set({'n', 't'}, '<C-k>', [[<CMD>wincmd k<CR>]], opts)
vim.keymap.set({'n', 't'}, '<C-l>', [[<CMD>wincmd l<CR>]], opts)

-- insert blank lines, using '[' and ']' as leader
vim.keymap.set('n', '[<space>', [[:set paste<CR>m`O<ESC>``:set nopaste<CR>]])
vim.keymap.set('n', ']<space>', [[:set paste<CR>m`o<ESC>``:set nopaste<CR>]])

-- remove blank lines, using '[' and ']' as leader
vim.keymap.set('n', '[d', [[m`:silent -g/\m^s*$/d<CR>``:noh<CR>]], opts)
vim.keymap.set('n', ']d', [[m`:silent +g/\m^\s*$/d<CR>``:noh<CR>]], opts)


vim.keymap.set('n', '[B', '<CMD>bfirst<CR>', opts)
vim.keymap.set('n', ']B', '<CMD>blast<CR>', opts)

vim.keymap.set('n', '[L', '<CMD>lfirst<CR>', opts)
vim.keymap.set('n', ']L', '<CMD>llast<CR>', opts)

vim.keymap.set('n', '[Q', '<CMD>cfirst<CR>', opts)
vim.keymap.set('n', ']Q', '<CMD>clast<CR>', opts)

vim.keymap.set('n', '[T', '<CMD>tfirst<CR>', opts)
vim.keymap.set('n', ']T', '<CMD>tlast<CR>', opts)

--TODO
-- exchange lines with [e and ]e

-- pass a count to an expression mapping
local function add_count(cmd)
  return "<CMD>echo " .. vim.v.count .. "<CMD>" .. cmd .. "<CR>"
end

opts = {silent = true, expr = true}

vim.keymap.set('n', '[b', add_count('bprev'), opts)
vim.keymap.set('n', ']b', add_count('bnext'), opts)

vim.keymap.set('n', '[l', add_count('lprev'), opts)
vim.keymap.set('n', ']l', add_count('lnext'), opts)

vim.keymap.set('n', '[q', add_count('cprev'), opts)
vim.keymap.set('n', ']q', add_count('cnext'), opts)

vim.keymap.set('n', '[t', add_count('tprev'), opts)
vim.keymap.set('n', ']t', add_count('tnext'), opts)
