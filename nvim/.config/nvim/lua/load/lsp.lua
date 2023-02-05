return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp", -- for defult capabilities
      "p00f/clangd_extensions.nvim", -- lsp extensions
      "MrcJkb/haskell-tools.nvim",
      "folke/neodev.nvim",
      "rmagatti/goto-preview", -- sourced in config
      "nvim-lua/plenary.nvim",
    },
    config = function()
      require("cfg.lsp")
    end,
  },
  {
    "kosayoda/nvim-lightbulb",
    dependencies = "antoinemadec/FixCursorHold.nvim",
    config = function()
      require("nvim-lightbulb").setup { autocmd = { enabled = true } }
      vim.fn.sign_define("LightBulbSign", { text = " " })
    end
  },
  { "j-hui/fidget.nvim", config = true },
  { "weilbith/nvim-code-action-menu", cmd = 'CodeActionMenu' },
  { "amrbashir/nvim-docs-view", cmd = "DocsViewToggle", config = true },
}
