return {
  {
    "nxvu699134/vn-night.nvim", -- color schemes
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd[[colorscheme vn-night]]
    end
  },
  {
    "ofirgall/ofirkai.nvim",
    "lourenci/github-colors",
  },
  {
    "uga-rosa/ccc.nvim", -- colorizer and color picker
    config = function()
      require("ccc").setup { highlighter = { auto_enable = true } }
    end,
  },
  {
    "karb94/neoscroll.nvim", -- window scrolling animations
    config = function()
      require("neoscroll").setup { easing_function = "sine" }
    end,
  },
  {
    "xiyaowong/nvim-transparent", -- transparent background
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
