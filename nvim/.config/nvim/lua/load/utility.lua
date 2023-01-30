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
