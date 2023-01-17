require("utils.map")
return {
  {
    "elihunter173/dirbuf.nvim",
    config = true,
    event = "BufWinEnter",
  },
  {
    "numToStr/Comment.nvim",
    config = true,
    event = "BufWinEnter",
  },
  {
    "chentoast/marks.nvim",
    config = true,
    event = "BufWinEnter",
  },
  {
    "tversteeg/registers.nvim",
    config = true,
    event = "BufWinEnter",
  },
  {
    "johnfrankmorgan/whitespace.nvim",
    config = function()
      require("cfg.whitespace")
    end,
    event = "BufWinEnter",
  },
  {
    "junegunn/vim-easy-align",
    config = function()
      nxmap('ga', '<Plug>(EasyAlign)')
    end,
    event = "BufWinEnter",
  },
}
