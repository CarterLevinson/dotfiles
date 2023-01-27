-- set some basic normal mode keymaps, mostly using leader
nmap("<leader>w", cmd "write")
nmap("<leader>c", cmd "close")
nmap("<space>", cmd "nohlsearch")
-- fast buffer switch
nmap("<leader>b", ":ls<CR>:b<space>")
-- pseudo auto close
imap("{<CR>", "{<CR>}<ESC>O")

-- follow :h Terminal and bring back the escape key
tmap("<ESC>", "<C-\\><C-n>")

-- better window movement bindings
ntmap("<C-h>", cmd "wincmd h")
ntmap("<C-j>", cmd "wincmd j")
ntmap("<C-k>", cmd "wincmd k")
ntmap("<C-l>", cmd "wincmd l")

nmap("[b", cmd "bprev")
nmap("]b", cmd "bnext")
nmap("[B", cmd "bfirst")
nmap("]B", cmd "blast")

nmap("[t", cmd "tprev")
nmap("]t", cmd "tnext")
nmap("[T", cmd "tfirst")
nmap("]T", cmd "tlast")

nmap("[l", cmd "lprev")
nmap("]l", cmd "lnext")
nmap("[L", cmd "lfirst")
nmap("]L", cmd "llast")

nmap("[q", cmd "cprev")
nmap("]q", cmd "cnext")
nmap("[Q", cmd "cfirst")
nmap("]Q", cmd "clast")

nmap("[d", vim.diagnostic.goto_prev)
nmap("]d", vim.diagnostic.goto_next)

-- insert blank lines, using '[' and ']' as leader
nmap("[<space>", [[:set paste<CR>m`O<ESC>``:set nopaste<CR>]])
nmap("]<space>", [[:set paste<CR>m`o<ESC>``:set nopaste<CR>]])

-- remove blank lines, using "[' and ']' as leader
nmap('[s', [[m`:silent -g/\m^s*$/d<CR>``:noh<CR>]])
nmap(']s', [[m`:silent +g/\m^\s*$/d<CR>``:noh<CR>]])

--TODO: keymaps to exchange lines with [e and ]e
