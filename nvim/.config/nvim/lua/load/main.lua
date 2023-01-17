require("utils.map")
return {
  "tpope/vim-eunuch",
  "chrisbra/unicode.vim",
  "mbbill/undotree",
  "aymericbeaumet/vim-symlink",
  "jghauser/mkdir.nvim",

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
    "radenling/vim-dispatch-neovim",
    dependencies = "tpope/vim-dispatch",
    config = function()
      vim.g.dispatch_no_maps = 1
    end,
    event = "CmdlineEnter",
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
    "miversen33/netman.nvim",
    config = function()
      require("netman")
    end,
    enabled = false,
  },
  {
    "superevilmegaco/AutoRemoteSync.nvim",
    config = function()
      require("cfg.sync")
    end,
  },
}
