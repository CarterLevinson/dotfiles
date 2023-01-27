return {
  "hrsh7th/nvim-cmp",
  dependencies = {
    "dcampos/nvim-snippy", -- snippets
    "dcampos/cmp-snippy",
    "hrsh7th/cmp-omni", -- main cmp src plugins
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-calc",
    "hrsh7th/cmp-cmdline",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-nvim-lsp-signature-help",
    "hrsh7th/cmp-nvim-lsp-document-symbol",
    -- "rcarriga/cmp-dap",
    "petertriho/cmp-git", -- extra cmp src plugins
    "ray-x/cmp-treesitter",
    "kristijanhusak/vim-dadbod-completion",
    "amarakon/nvim-cmp-lua-latex-symbols",
    "lukas-reineke/cmp-under-comparator",
  },
  config = function()
    require("cfg.cmp")
  end,
}
