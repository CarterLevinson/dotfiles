return {
  {
    "folke/tokyonight.nvim", -- color schemes
    "tanvirtin/monokai.nvim",
    "sainnhe/sonokai",
    "projekt0n/github-nvim-theme",
  },
  {
    "goolord/alpha-nvim", -- dashboard
    config = function()
      require("cfg.alpha")
    end,
    enabled = false,
  },
  {
    "anuvyklack/windows.nvim", -- window plugins
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
    "karb94/neoscroll.nvim",
    config = function()
      require("neoscroll").setup { easing_function = "sine" }
    end,
  },
  {
    "xiyaowong/nvim-transparent",
    config = function()
      require("transparent").setup { enable = true }
    end,
  },
}
