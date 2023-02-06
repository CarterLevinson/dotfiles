return {
  {
    "catppuccin/nvim",
    lazy = false,
    priority = 1000,
    name = "catppuccin",
    config = function()
      vim.cmd.colorscheme [[catppuccin]]
    end
  },
  {
    "ofirgall/ofirkai.nvim", -- alt colorschemes
    "shaunsingh/moonlight.nvim",
    "lourenci/github-colors",
    "bluz71/vim-nightfly-colors",
  },
  {
    "uga-rosa/ccc.nvim", -- colorizer and color picker
    opts = { highlighter = { auto_enable = true } },
    event = "BufWinEnter",
  },
  {
    "karb94/neoscroll.nvim", -- window scrolling animations
    opts = { easing_function = "sine" },
    event = "BufWinEnter",
  },
  {
    "xiyaowong/nvim-transparent", -- transparent background
    opts = { enable = true },
    event = "BufWinEnter",
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
    event = "BufWinEnter",
    enabled = false,
  },
}
