return {
  "chrisbra/csv.vim", -- tabular data

  "vim-scripts/cscope.vim", -- C/C++
  "vim-scripts/cpp_cppcheck.vim",
  "vim-scripts/a.vim",

  "mpickering/hlint-refactor-vim", -- Haskell
  "neovimhaskell/haskell-vim",
  "vmchale/pointfree",
  "Twinside/vim-hoogle",

  "mboughaba/i3config.vim", -- syntax files
  "Fymyte/mbsync.vim",
  "kmonad/kmonad-vim",
  "jbmorgado/vim-pine-script",
  "fladson/vim-kitty",

  "jghauser/follow-md-links.nvim", -- markdown

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
  },
  {
    "lervag/vimtex", -- LaTex
    config = function()
      vim.g.vimtex_inded_enabled = 1
      vim.g.vimtex_complete_closed_braces = 1
      vim.g.vimtex_view_method = get_filename(os.getenv("READER"))
    end,
  },
}
