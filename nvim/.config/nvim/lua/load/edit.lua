return {
  { "elihunter173/dirbuf.nvim", config = true },
  { "numToStr/Comment.nvim", config = true },
  { "kylechui/nvim-surround", config = true },
  { "chentoast/marks.nvim", config = true },
  { "tversteeg/registers.nvim", config = true },
  {
    "johnfrankmorgan/whitespace.nvim",
    config = function()
      require("cfg.whitespace")
    end,
  },
  {
    "junegunn/vim-easy-align",
    config = function()
      nxmap('ga', '<Plug>(EasyAlign)')
    end,
  },
}
