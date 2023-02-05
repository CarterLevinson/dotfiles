return {
  {
    "nvim-treesitter/nvim-treesitter",
    config = function()
      require("cfg.treesitter")
    end,
    build = ":TSUpdate",
    event = "BufWinEnter",
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    dependencies = "nvim-treesitter",
  },
  {
    "nvim-treesitter/nvim-treesitter-refactor",
    dependencies = "nvim-treesitter",
  },
  {
    "nvim-treesitter/playground",
    dependencies = "nvim-treesitter",
    cmd = "TSPlaygroundToggle",
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    dependencies = "nvim-treesitter",
    config = true,
    enabled = false,
  },
  {
    "windwp/nvim-ts-autotag",
    dependencies = "nvim-treesitter",
    config = true,
  },
  {
    "danymat/neogen", -- create doc strings
    dependencies = "nvim-treesitter",
    opts = { snippet_engine = "snippy" },
    cmd = "Neogen",
  },
}
