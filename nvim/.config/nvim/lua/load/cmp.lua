return {
  "hrsh7th/nvim-cmp",
  dependencies = {
    "dcampos/nvim-snippy",
    "dcampos/cmp-snippy",
    "hrsh7th/cmp-omni",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-calc",
    "hrsh7th/cmp-cmdline",
    "hrsh7th/cmp-nvim-lua",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-nvim-lsp-signature-help",
    "hrsh7th/cmp-nvim-lsp-document-symbol",
    "rcarriga/cmp-dap",
    "petertriho/cmp-git",
    "kristijanhusak/vim-dadbod-completion",
    "amarakon/nvim-cmp-lua-latex-symbols",
    "lukas-reineke/cmp-under-comparator",
    "onsails/lspkind.nvim",
  },
  config = function()
    require("cfg.cmp")
  end,
}
