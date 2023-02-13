return {
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp", -- for defult capabilities
            "simrat39/rust-tools.nvim",
            "rmagatti/goto-preview",
            "folke/neodev.nvim",
            "p00f/clangd_extensions.nvim", -- lsp extensions
            {
                "MrcJkb/haskell-tools.nvim",
                dependencies = "nvim-lua/plenary.nvim",
            },
        },
        config = function()
          require "cfg.lsp"
        end,
    },
    {
        "j-hui/fidget.nvim",
        config = true
    },
    {
        "weilbith/nvim-code-action-menu",
        cmd = 'CodeActionMenu'
    },
    {
        "amrbashir/nvim-docs-view",
        cmd = "DocsViewToggle",
        config = true
    },
    {
        "kosayoda/nvim-lightbulb",
        dependencies = "antoinemadec/FixCursorHold.nvim",
        config = function()
          require "nvim-lightbulb".setup { autocmd = { enabled = true } }
          vim.fn.sign_define("LightBulbSign", { text = " " })
        end
    },
}
