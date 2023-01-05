local cmp = require('cmp')
local cmp_autopairs = require('nvim-autopairs.completion.cmp')
local auto_pairs = require('nvim-autopairs')
auto_pairs.setup{}
cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
