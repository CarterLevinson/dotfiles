local set = vim.opt
-- reasonable values between 5 and 20 should be okay
set.winwidth = 10
set.winminwidth = 5
set.equalalways = false

require('windows').setup{
  ignore = {
    filetype = {
      "vista",
      "vista_kind",
    }
  },
}
