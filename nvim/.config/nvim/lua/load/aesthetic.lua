return {
    {
        "rebelot/kanagawa.nvim", -- main colorscheme
        lazy = false,
        priority = 1000,
        config = function()
          vim.cmd.colorscheme [[kanagawa]]
        end
    },
    {
        "ofirgall/ofirkai.nvim", -- alt colorschemes
        "tanvirtin/monokai.nvim",
        "sainnhe/sonokai",
        "lourenci/github-colors",
    },
    {
        "uga-rosa/ccc.nvim", -- colorizer and color picker
        opts = { highlighter = { auto_enable = true } },
        event = "BufWinEnter",
    },
    {
        "karb94/neoscroll.nvim", -- window scrolling animations
        opts = { easing_function = "sine" },
        event = "BufWinEnter",
    },
    {
        "xiyaowong/nvim-transparent", -- provides transparent background hl
        opts = { enable = true },
        event = "BufWinEnter",
    },
    {
        "anuvyklack/windows.nvim", -- window animation and auto resize
        dependencies = {
            "anuvyklack/middleclass",
            "anuvyklack/animation.nvim",
        },
        config = function()
          vim.opt.winwidth = 10
          vim.opt.winminwidth = 5
          vim.opt.equalalways = false
          require "windows".setup {}
        end,
        event = "BufWinEnter",
        enabled = false,
    },
}
