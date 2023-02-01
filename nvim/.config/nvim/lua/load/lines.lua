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
}
