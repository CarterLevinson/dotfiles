require("toggleterm").setup {
  open_mapping = "<leader>t",

  -- insert_mapping = true,
  terminal_mapping = true,

  persist_mode = true,
  close_on_exit = true,

  shade_terminals = true,

  float_opts = { border = "curved" },
}

-- toggle term keymaps
nmap("<leader>tt", cmd "ToggleTermSendCurrentLine")
nmap("<leader>tv", cmd "ToggleTermSendVisualSelection")
nmap("<leader>tV", cmd "ToggleTermSendVisualLines")
nmap("<leader>ta", cmd "ToggleTermToggleAll")
