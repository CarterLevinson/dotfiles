local path = vim.fn.stdpath('data') .. '/site/pack/packer'
local himalaya = path .. '/start/himalaya/vim/plugin/himalaya.vim'
vim.cmd('source' .. ' ' .. himalaya)
vim.g.himalaya_mailbox_picker = 'telescope'
vim.g.himalaya_telescope_preview_enabled = 0
