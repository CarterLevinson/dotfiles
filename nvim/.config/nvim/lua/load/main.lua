return {
  {
    "ibhagwan/fzf-lua", -- fuzzy finder
    config = function()
      require("cfg.fzf")
    end
  },
  {
    "akinsho/toggleterm.nvim", -- better terminal integration
    config = function()
      require("cfg.toggleterm")
    end,
    version = "v2.*",
  },
  {
    "liuchengxu/vista.vim", -- lsp symbol browser
    config = function()
      require("cfg.vista")
    end,
  },
  {
    "radenling/vim-dispatch-neovim", -- async :make
    dependencies = "tpope/vim-dispatch",
    config = function()
      vim.g.dispatch_no_maps = 1
    end,
    event = "CmdlineEnter",
  },
  {
    "danymat/neogen", -- create doc strings
    dependencies = "nvim-treesitter/nvim-treesitter",
    config = function()
      require("neogen").setup { snippet_engine = "snippy" }
    end,
    event = "CmdlineEnter",
  },
  {
    "notjedi/nvim-rooter.lua", -- auto cd to projec root directory
    config = function()
      require("nvim-rooter").setup { manual = false }
    end,
  },
  {
    "tpope/vim-eunuch", -- misc but useful
    "vim-scripts/unicode.vim",
    "mbbill/undotree",
    "aymericbeaumet/vim-symlink",
    "jghauser/mkdir.nvim",
  },
}
