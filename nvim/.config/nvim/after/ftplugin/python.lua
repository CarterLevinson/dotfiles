local setlocal       = vim.bo
setlocal.tabstop     = 4
setlocal.shiftwidth  = 4
setlocal.softtabstop = 4
setlocal.formatprg   = 'yapf'

-- custom user commands based off toggle term functionality
vim.cmd[[com! -nargs=1 Pyman
\ :13TermEx direction=horizontal size=50 go_back=0 cmd='pydoc <args> | less'<CR>
]]
