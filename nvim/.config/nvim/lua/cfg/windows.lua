local set = vim.opt
set.winwidth = 10
set.winminwidth = 5
set.equalalways = false

require('windows').setup {
  autowidth = { enable = true },
  ignore = { filetype = { "vista", "vista_kind" }, buftype = { "nofile" }  },
}
