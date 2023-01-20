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
      NXmap('ga', '<Plug>(EasyAlign)')
    end,
  },
  {
    "alpertuna/vim-header",
    config = function()
      vim.g.header_field_author        = "Carter S. Levinson"
      vim.g.header_field_author_email  = "cslevo@posteo.net"
      vim.g.header_field_filename_path = 1
      vim.g.header_field_modified_by   = 0
      vim.g.header_auto_update_header  = 1
    end
  },
}
