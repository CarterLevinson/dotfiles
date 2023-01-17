local setlocal = vim.bo
setlocal.cinoptions = 'L0'

-- custom user commands based off toggle term functionality
vim.cmd[[com! -nargs=1 Cppman
\ :10TermEx direction=horizontal size=50 go_back=0 cmd='cppman <args>'<CR>
]]

