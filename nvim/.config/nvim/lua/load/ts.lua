return {
  {
    "nvim-treesitter/nvim-treesitter",
    config = function()
      require("cfg.ts")
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
    event = "CmdlineEnter",
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    dependencies = "nvim-treesitter",
    config = function()
      require("treesitter-context").setup {}
    end,
  },
  {
    "windwp/nvim-ts-autotag",
    dependencies = "nvim-treesitter",
    config = function()
      require("nvim-ts-autotag").setup {}
    end,
  },
}
