require("utils.map")
return {
  "tpope/vim-eunuch",
  "chrisbra/unicode.vim",
  "mbbill/undotree",
  "aymericbeaumet/vim-symlink",
  "jghauser/mkdir.nvim",
  "tpope/vim-fugitive",
  {
    "lewis6991/gitsigns.nvim",
    version = "release",
    config = function()
      require("gitsigns").setup{}
    end,
    event = "BufWinEnter",
  },
  {
    "ibhagwan/fzf-lua",
    dependencies = "kyazdani42/nvim-web-devicons",
    config = function()
      require("cfg.fzf")
    end,
  },
  {
    "liuchengxu/vista.vim",
    config = function()
      require("cfg.vista")
    end,
  },
  {
    "akinsho/toggleterm.nvim",
    config = function()
      require("cfg.toggleterm")
    end,
    version = "v2.*",
  },
  {
    "danymat/neogen",
    dependencies = "nvim-treesitter/nvim-treesitter",
    config = function()
      require("neogen").setup{
        snippet_engine = "snippy"
      }
    end,
    event = "CmdlineEnter",
  },
  {
    "miversen33/netman.nvim",
    config = function()
      require("netman")
    end,
    enabled = false,
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
    "superevilmegaco/AutoRemoteSync.nvim",
    config = function()
      require("cfg.sync")
    end,
  },
  {
    "notjedi/nvim-rooter.lua",
    config = function()
      require("nvim-rooter").setup{
        manual = false
      }
    end,
  },
  {
    "dhruvasagar/vim-prosession",
    dependencies = "tpope/vim-obession",
    config = function()
      vim.g.prosession_default_session = 1
      vim.g.prosession_per_branch = 0
      vim.g.prosession_dir = vim.fn.stdpath("data") .. "/sessions"
      vim.cmd[[let g:Prosession_ignore_expr = {-> !isdirectory('.git')}]]
    end,
    enabled = false,
  },
}
