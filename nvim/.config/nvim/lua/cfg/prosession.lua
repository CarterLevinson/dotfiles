vim.g.prosession_dir = '~/.local/share/nvim/sessions'
vim.g.prosession_per_branch = 1
vim.g.prosession_default_session = 1
vim.cmd[[
let g:Prosession_ignore_expr = {-> !isdirectory('.git')}
]]
