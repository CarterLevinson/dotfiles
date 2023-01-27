return {
  {
    "nvim-lualine/lualine.nvim",
    dependencies = "kyazdani42/nvim-web-devicons",
    config = function()
      require("cfg.lualine")
    end,
    event = "BufWinEnter",
  },
  {
    "kdheepak/tabline.nvim",
    dependencies = "kyazdani42/nvim-web-devicons",
    config = true,
    event = "BufWinEnter",
  },
}
