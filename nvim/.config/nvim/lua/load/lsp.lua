return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "hrsh7th/nvim-cmp",
      "p00f/clangd_extensions.nvim",
      "MrcJkb/haskell-tools.nvim",
      "folke/neodev.nvim",
      "rmagatti/goto-preview",
      "nvim-lua/plenary.nvim",
    },
    config = function()
      require("cfg.lsp")
    end,
  },
  { "j-hui/fidget.nvim", config = true },
  { "smjonas/inc-rename.nvim", config = true },
  { "weilbith/nvim-code-action-menu", cmd = 'CodeActionMenu' },
  {
    "kosayoda/nvim-lightbulb",
    dependencies = "antoinemadec/FixCursorHold.nvim",
    config = function()
      require("nvim-lightbulb").setup { autocmd = { enabled = true } }
    end
  },
  {
    "folke/trouble.nvim",
    dependencies = "kyazdani42/nvim-web-devicons",
    config = function()
      require("cfg.trouble")
    end,
  },
  {
    "liuchengxu/vista.vim", -- lsp symbol browser
    config = function()
      require("cfg.vista")
    end,
  },
}
