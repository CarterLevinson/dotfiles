require("trouble").setup{}
--set up trouble keybindings

-- normal mode
nmap("<leader>x",  cmd "TroubleToggle")
nmap("<leader>xw", cmd "Trouble workspace_diagnostics")
nmap("<leader>xl", cmd "Trouble loclist")
nmap("<leader>xq", cmd "Trouble quickfix")
nmap("<leader>xr", cmd "Trouble lsp_references")
nmap("<leader>xc", cmd "TroubleClose")
nmap("<leader>xt", cmd "TroubleToggle")
-- vim.keymap.set("n", "gR",         "<CMD>Trouble lsp_references<CR>", opts)

-- command aliases
vim.cmd[[cnoreabbrev copen Trouble quickfix]]
vim.cmd[[cnoreabbrev cclose TroubleClose]]
vim.cmd[[cnoreabbrev lopen Trouble loclist]]
vim.cmd[[cnoreabbrev lclose TroubleClose]]
