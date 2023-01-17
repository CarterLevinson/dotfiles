require("utils.map")
-- set some basic normal mode keymaps, mostly using leader
nmap('<space>', ':nohlsearch<CR>')
nmap('<leader>bd', ':ls<CR>:b<space>')
nmap('<leader>s', ':w<CR>')

-- pseudo auto close
imap('{<CR>', '{<CR>}<ESC>O')
-- imap('<C-a>', '<ESC>a')
-- imap([[']], [['<ESC>ylpi]])
-- imap([["]], [["<ESC>ylpi]])

-- follow :h Terminal and bring back the escape key
tmap('<ESC>', '<C-\\><C-n>')

-- better window movement bindings
ntmap('<C-h>', '<CMD>wincmd h<CR>')
ntmap('<C-j>', '<CMD>wincmd j<CR>')
ntmap('<C-k>', '<CMD>wincmd k<CR>')
ntmap('<C-l>', '<CMD>wincmd l<CR>')

nmap('[b', '<CMD>bprev<CR>')
nmap(']b', '<CMD>bnext<CR>')
nmap('[B', '<CMD>bfirst<CR>')
nmap(']B', '<CMD>blast<CR>')

nmap('[l', '<CMD>lprev<CR>')
nmap(']l', '<CMD>lnext<CR>')
nmap('[L', '<CMD>lfirst<CR>')
nmap(']L', '<CMD>llast<CR>')

nmap('[q', '<CMD>cprev<CR>')
nmap(']q', '<CMD>cnext<CR>')
nmap('[Q', '<CMD>cfirst<CR>')
nmap(']Q', '<CMD>clast<CR>')

nmap('[t', '<CMD>tprev<CR>')
nmap(']t', '<CMD>tnext<CR>')
nmap('[T', '<CMD>tfirst<CR>')
nmap(']T', '<CMD>tlast<CR>')

--TODO: exchange lines with [e and ]e

-- remove blank lines, using '[' and ']' as leader
nmap('[d', [[m`:silent -g/\m^s*$/d<CR>``:noh<CR>]])
nmap(']d', [[m`:silent +g/\m^\s*$/d<CR>``:noh<CR>]])

-- insert blank lines, using '[' and ']' as leader
nmap('[<space>', [[:set paste<CR>m`O<ESC>``:set nopaste<CR>]])
nmap(']<space>', [[:set paste<CR>m`o<ESC>``:set nopaste<CR>]])
