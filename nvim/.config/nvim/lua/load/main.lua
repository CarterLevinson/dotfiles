return {
  {
    "ibhagwan/fzf-lua",
    config = function()
      require("cfg.fzf")
    end
  },
  {
    "akinsho/toggleterm.nvim",
    config = function()
      require("cfg.toggleterm")
    end,
    version = "v2.*",
  },
  {
    "radenling/vim-dispatch-neovim",
    dependencies = "tpope/vim-dispatch",
    config = function()
      vim.g.dispatch_no_maps = 1
    end,
    event = "CmdlineEnter",
  },
  {
    "liuchengxu/vista.vim",
    config = function()
      require("cfg.vista")
    end,
  },
  {
    "danymat/neogen",
    dependencies = "nvim-treesitter/nvim-treesitter",
    config = function()
      require("neogen").setup { snippet_engine = "snippy" }
    end,
    event = "CmdlineEnter",
  },
  {
    "notjedi/nvim-rooter.lua",
    config = function()
      require("nvim-rooter").setup { manual = false }
    end,
  },
  {
    "superevilmegaco/AutoRemoteSync.nvim",
    config = function()
      require("cfg.sync")
    end,
  },
  {
    "tpope/vim-eunuch",
    "chrisbra/unicode.vim",
    "mbbill/undotree",
    "aymericbeaumet/vim-symlink",
    "jghauser/mkdir.nvim",
  },
  {
    "miversen33/netman.nvim",
    config = function()
      require("netman")
    end,
    enabled = false,
  },
}
