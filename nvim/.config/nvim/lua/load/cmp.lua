return {
  "hrsh7th/nvim-cmp",
  dependencies = {
    "dcampos/nvim-snippy", -- snippets
    "dcampos/cmp-snippy",
    "hrsh7th/cmp-omni", -- main cmp src plugins
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-calc",
    "hrsh7th/cmp-cmdline",
    -- "hrsh7th/cmp-nvim-lua",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-nvim-lsp-signature-help",
    "hrsh7th/cmp-nvim-lsp-document-symbol",
    "rcarriga/cmp-dap", -- extra cmp src plugins
    "petertriho/cmp-git",
    "kristijanhusak/vim-dadbod-completion",
    "amarakon/nvim-cmp-lua-latex-symbols",
    "lukas-reineke/cmp-under-comparator",
    "onsails/lspkind.nvim", -- menu setup
  },
  config = function()
    require("cfg.cmp")
  end,
}
