require("utils.map")
return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "hrsh7th/nvim-cmp",
      "p00f/clangd_extensions.nvim",
      "simrat39/rust-tools.nvim",
      {
        "MrcJkb/haskell-tools.nvim",
        dependencies = {
          "akinsho/toggleterm.nvim",
          "nvim-lua/plenary.nvim",
        },
      },
    },
    config = function()
      require("cfg.lsp")
    end,
  },
  {
    "kosayoda/nvim-lightbulb",
    dependencies = "antoinemadec/FixCursorHold.nvim",
    config = function()
      require('nvim-lightbulb').setup{
        autocmd = {enabled = true}
      }
    end
  },
  {
    "weilbith/nvim-code-action-menu",
    cmd = "CodeActionMenu"
  },
  {
    "smjonas/inc-rename.nvim",
    config = function()
      require("inc_rename").setup{}
    end,
  },
  {
    "rmagatti/goto-preview",
    config = function()
      require("goto-preview").setup{}
    end
  },
  {
    "folke/trouble.nvim",
    dependencies = "kyazdani42/nvim-web-devicons",
    config = function()
      require("cfg.trouble")
    end,
  }
}
