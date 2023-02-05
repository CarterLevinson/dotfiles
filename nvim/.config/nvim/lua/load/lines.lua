return {
  {
    "nvim-lualine/lualine.nvim",
    dependencies = {
      "kyazdani42/nvim-web-devicons",
      "kdheepak/tabline.nvim",
    },
    config = function()
      require("cfg.lines")
    end,
    event = "BufWinEnter",
  },
  {
    "utilyre/barbecue.nvim",
    version = "*",
    dependencies = {
      "SmiteshP/nvim-navic",
      "kyazdani42/nvim-web-devicons",
    },
    opts = {
      theme = {
        dirname = { fg = "#FAF9F6" },
        basename = { bold = true },
        context = {},
      },
    },
  }
}
