local set = vim.opt
-- reasonable values between 5 and 20 should be okay
set.winwidth = 10
set.winminwidth = 10
set.equalalways = false

require('windows').setup{
  autowidth = {
  },
  ignore = {
    filetype = {
      'vista',
      'vista_kind',
      'vista_markdown',
    },
  },
}
