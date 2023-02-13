require "trouble".setup {}

-- Trouble keymaps
nmap("<leader>x",  cmd "TroubleToggle")
nmap("<leader>xw", cmd "Trouble workspace_diagnostics")
nmap("<leader>xl", cmd "Trouble loclist")
nmap("<leader>xq", cmd "Trouble quickfix")
nmap("<leader>xr", cmd "Trouble lsp_references")
nmap("<leader>xc", cmd "TroubleClose")
nmap("<leader>xt", cmd "TroubleToggle")

-- command aliases
vim.cmd [[cnoreabbrev copen Trouble quickfix]]
vim.cmd [[cnoreabbrev cclose TroubleClose]]
vim.cmd [[cnoreabbrev lopen Trouble loclist]]
vim.cmd [[cnoreabbrev lclose TroubleClose]]
