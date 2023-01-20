require("trouble").setup{}
--set up trouble keybindings

-- normal mode
Nmap("<leader>x",  Cmd "TroubleToggle")
Nmap("<leader>xw", Cmd "Trouble workspace_diagnostics")
Nmap("<leader>xl", Cmd "Trouble loclist")
Nmap("<leader>xq", Cmd "Trouble quickfix")
Nmap("<leader>xr", Cmd "Trouble lsp_references")
Nmap("<leader>xc", Cmd "TroubleClose")
Nmap("<leader>xt", Cmd "TroubleToggle")
-- vim.keymap.set("n", "gR",         "<CMD>Trouble lsp_references<CR>", opts)

-- command aliases
vim.cmd[[cnoreabbrev copen Trouble quickfix]]
vim.cmd[[cnoreabbrev cclose TroubleClose]]
vim.cmd[[cnoreabbrev lopen Trouble loclist]]
vim.cmd[[cnoreabbrev lclose TroubleClose]]
