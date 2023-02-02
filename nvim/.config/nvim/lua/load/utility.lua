return {
  {
    "akinsho/toggleterm.nvim", -- better terminal integration
    config = function()
      require("cfg.toggleterm")
    end,
    version = "v2.*",
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
    "kristijanhusak/vim-dadbod-ui",
    dependencies = "tpope/vim-dadbod",
    cmd = { "DB", "DBUI" },
  },
  {
    "ibhagwan/fzf-lua", -- fuzzy finder
    dependencies = "kyazdani42/nvim-web-devicons",
    config = function()
      require("cfg.fzf")
    end
  },
  {
    "notjedi/nvim-rooter.lua", -- auto cd to projec root directory
    opts = { manual = false },
  },
  {
    "folke/trouble.nvim", -- diagnostic interface
    dependencies = "kyazdani42/nvim-web-devicons",
    config = function()
      require("cfg.trouble")
    end,
    enabled = false,
  },
  {
    "liuchengxu/vista.vim", -- symbol browser
    config = function()
      require("cfg.vista")
    end,
  },
  {
    "knubie/vim-kitty-navigator",
    build = "cp ./*.py ~/.config/kitty/"
  },
  {
    "tpope/vim-eunuch", -- misc but useful
    "mbbill/undotree",
    -- "vim-scripts/unicode.vim",
    "aymericbeaumet/vim-symlink",
    "jghauser/mkdir.nvim",
  },
}
