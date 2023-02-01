return {
  "mboughaba/i3config.vim", -- syntax highlighting
  "Fymyte/mbsync.vim",
  "kmonad/kmonad-vim",
  "jbmorgado/vim-pine-script",
  "fladson/vim-kitty",

  {
    "chrisbra/csv.vim", -- tabular data
    ft = { "csv", "tsv" }
  },

  {
    "vim-scripts/cscope.vim", -- C/C++
    ft = { "c", "cpp" }
  },

  {
    "vim-scripts/a.vim",
    ft = { "c", "cpp" }
  },

  {
    "mpickering/hlint-refactor-vim", -- Haskell
    ft = { "haskell", "lhaskell" }
  },

  {
    "neovimhaskell/haskell-vim",
    ft = { "haskell", "lhaskell" }
  },

  {
    "vmchale/pointfree",
    ft = { "haskell", "lhaskell" }
  },

  {
    "Twinside/vim-hoogle",
    ft = { "haskell", "lhaskell" }
  },

  {
    "jghauser/follow-md-links.nvim", -- Markdown
    ft = { "markdown", "rmd" }
  },

  {
    "iamcco/markdown-preview.nvim",
    config = function()
      vim.g.mkdp_auto_start = 1
      vim.g.mkdp_auto_close = 1
      vim.g.mkdp_theme = "dark"
      vim.g.mkdp_browser = get_filename(os.getenv("BROWSER"))
    end,
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
    ft = { "markdown", "rmd" },
  },

  {
    "lervag/vimtex", -- LaTeX
    config = function()
      vim.g.vimtex_inded_enabled = 1
      vim.g.vimtex_complete_closed_braces = 1
      vim.g.vimtex_view_method = get_filename(os.getenv("READER"))
    end,
    ft = { "tex" },
  },
}
