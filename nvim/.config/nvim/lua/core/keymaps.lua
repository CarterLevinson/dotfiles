-- set some basic normal mode keymaps, mostly using leader
Nmap("<space>", ":nohlsearch<CR>")
Nmap("<leader>b", ":ls<CR>:b<space>")
Nmap("<leader>s", ":w<CR>")

-- pseudo auto close
Imap("{<CR>", "{<CR>}<ESC>O")
-- imap('<C-a>', '<ESC>a')
-- imap([[']], [['<ESC>ylpi]])
-- imap([["]], [["<ESC>ylpi]])

-- follow :h Terminal and bring back the escape key
Tmap("<ESC>", "<C-\\><C-n>")

-- better window movement bindings
NTmap("<C-h>", "<CMD>wincmd h<CR>")
NTmap("<C-j>", "<CMD>wincmd j<CR>")
NTmap("<C-k>", "<CMD>wincmd k<CR>")
NTmap("<C-l>", "<CMD>wincmd l<CR>")

Nmap("[b", "<CMD>bprev<CR>")
Nmap("]b", "<CMD>bnext<CR>")
Nmap("[B", "<CMD>bfirst<CR>")
Nmap("]B", "<CMD>blast<CR>")

Nmap("[l", "<CMD>lprev<CR>")
Nmap("]l", "<CMD>lnext<CR>")
Nmap("[L", "<CMD>lfirst<CR>")
Nmap("]L", "<CMD>llast<CR>")

Nmap("[q", "<CMD>cprev<CR>")
Nmap("]q", "<CMD>cnext<CR>")
Nmap("[Q", "<CMD>cfirst<CR>")
Nmap("]Q", "<CMD>clast<CR>")

Nmap("[t", "<CMD>tprev<CR>")
Nmap("]t", "<CMD>tnext<CR>")
Nmap("[T", "<CMD>tfirst<CR>")
Nmap("]T", "<CMD>tlast<CR>")

Nmap("[d", vim.diagnostic.goto_prev)
Nmap("]d", vim.diagnostic.goto_next)
--TODO: exchange lines with [e and ]e

-- remove blank lines, using "[' and ']' as leader
-- Nmap('[d', [[m`:silent -g/\m^s*$/d<CR>``:noh<CR>]])
-- Nmap(']d', [[m`:silent +g/\m^\s*$/d<CR>``:noh<CR>]])

-- insert blank lines, using '[' and ']' as leader
Nmap("[<space>", [[:set paste<CR>m`O<ESC>``:set nopaste<CR>]])
Nmap("]<space>", [[:set paste<CR>m`o<ESC>``:set nopaste<CR>]])
