require("win_resize").setup{
  exclude_names = {
    "__vista__",
    "fzf",
  },
  exclude_fts = {
    "vista",
    "vista_kind",
    "fzf",
    "undotree",
  }
}

local resizeWin = vim.api.nvim_create_augroup("resizeWin", {clear = true})
vim.api.nvim_create_autocmd({"BufEnter", "FocusGained", "VimResized"}, {
  command = [[lua require("win_resize").resize()]],
  group = resizeWin,
})
