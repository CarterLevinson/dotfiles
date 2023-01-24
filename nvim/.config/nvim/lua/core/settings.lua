local set                 = vim.opt
local g                   = vim.g

g.loaded_python3_provider = 0
g.loaded_ruby_provider    = 0
g.loaded_perl_provider    = 0
g.loaded_node_provider    = 0

g.loaded_netrw            = 1
g.loaded_netrwPlugin      = 1
g.loaded_netrwSettings    = 1

set.spelllang             = "en_us"
set.wildignore            = {
  "*.o",
  "*.so",
  "*.pyc",
  "*.class",
  "*.hi"
}

set.confirm               = true
set.vb                    = true

set.browsedir             = "buffer"
set.diffopt               = "vertical"

-- display
set.title                 = true
set.showcmd               = true
set.wildmenu              = true
set.ruler                 = true
set.relativenumber        = true

set.laststatus            = 3
set.showtabline           = 2

set.lazyredraw            = true
-- line breaks
set.tw                    = 80
set.linebreak             = true
set.breakindent           = true

-- search
set.hlsearch              = true
set.incsearch             = true
set.ignorecase            = true
set.smartcase             = true

-- tabs/indents
set.tabstop               = 2
set.shiftwidth            = 2
set.softtabstop           = -1
set.expandtab             = true

set.autoindent            = true
set.smartindent           = true

-- set custom listchars
set.listchars             = {
   eol                    = "↲",
   tab                    = "»·",
   space                  = "␣",
   trail                  = "·",
   extends                = "☛",
   precedes               = "☚",
   conceal                = "┊",
   nbsp                   = "☠",
}

-- :grep to use ripgrep
vim.cmd [[set grepprg=rg\ --vimgrep\ --no-heading]]
vim.cmd [[set grepformat=%f:%l:%c:%m,%f:%l:%m]]

-- set default colorscheme
vim.cmd [[colorscheme aurora]]

-- treesitter fold settings ?
-- set.foldmethod = "expr"
-- set.foldexpr = "nvim_treesitter#foldexpr()"
-- start every file with folds open
-- autocmd BufReadPost,FileReadPost * normal zR
