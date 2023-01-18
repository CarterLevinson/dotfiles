return {
  {
    "karb94/neoscroll.nvim",
    config = function()
      require("neoscroll").setup{
        easing_function = "sine"
      }
    end,
  },
  {
    "xiyaowong/nvim-transparent",
    config = function()
      require("transparent").setup{
        enable = true
      }
    end,
  },
  {
    "anuvyklack/windows.nvim",
    dependencies = {
      "anuvyklack/middleclass",
      "anuvyklack/animation.nvim",
    },
    config = function()
      require("cfg.windows")
    end,
    enabled = false,
  },
  {
    "eightpigs/win_resize.nvim",
    config = function()
      require("cfg.resize")
    end
  },
  {
    "goolord/alpha-nvim",
    config = function()
      require("cfg.alpha")
    end,
    enabled = false,
  }
}
