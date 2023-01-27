return {
  {
    "nxvu699134/vn-night.nvim",
    "ofirgall/ofirkai.nvim",
    "lourenci/github-colors",
  },
  { "uga-rosa/ccc.nvim", config = true },
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
}
