require("toggleterm").setup{
  -- direction = "float",
  open_mapping = "<leader>t",

  insert_mapping = true,
  terminal_mapping = true,

  persist_mode = true,
  close_on_exit = true,


  shade_terminals = true,
  float_opts = {
    border = "curved",
  },
}

--/keymaps
Nmap("<leader>tt", Cmd "ToggleTermSendCurrentLine")
Nmap("<leader>tv", Cmd "ToggleTermSendVisualSelection")
Nmap("<leader>tV", Cmd "ToggleTermSendVisualLines")
Nmap("<leader>ta", Cmd "ToggleTermToggleAll")
Tmap("<leader>tc", Cmd "close")
