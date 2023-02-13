return {
    {
        "nvim-lualine/lualine.nvim",
        dependencies = {
            "kyazdani42/nvim-web-devicons",
            { "kdheepak/tabline.nvim", enabled = false },
        },
        config = function()
          require("cfg.lines")
        end,
        event = "UiEnter",
        enabled = true,
    },
    {
        "crispgm/nvim-tabline",
        dependencies = "kyazdani42/nvim-web-devicons",
        event = "UiEnter",
        opts = { show_icon = true },
        enabled = true,
    },
    {
        "romgrk/barbar.nvim",
        dependencies = "kyazdani42/nvim-web-devicons",
        event = "UiEnter",
        config = true,
        enabled = false
    },
    {
        "rebelot/heirline.nvim",
        event = "UiEnter",
        config = function()
          require('cfg.heirline')
        end,
        enabled = false,
    },
    {
        "utilyre/barbecue.nvim",
        version = "*",
        dependencies = {
            "SmiteshP/nvim-navic",
            "kyazdani42/nvim-web-devicons",
        },
        opts = { theme = "kanagawa" },
    }
}
