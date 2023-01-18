local set = vim.opt
-- reasonable values between 5 and 20 should be okay
set.winwidth = 10
set.winminwidth = 10
set.equalalways = false

require('windows').setup{
  autowidth = {
    filetype={
      vista = 1,
      vista_kind = 1,
      undotree = 1,
    }
  },
  ignore = {
    buftype = {
      "quickfix",
      "nofile",
    },
    filetype = {
      "vista",
      "vista_kind",
      "undotree",
    }
  },
}
