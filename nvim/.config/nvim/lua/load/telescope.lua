return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = "nvim-lua/plenary.nvim",
    config = function()
      require("cfg.telescope")
    end,
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    dependencies = "nvim-telescope/telescope.nvim",
    config = function()
      require("telescope").load_extension("fzf")
    end,
    build = "make",
  },
  {
    "nvim-telescope/telescope-github.nvim",
    dependencies = "nvim-telescope/telescope.nvim",
    config = function()
      require("telescope").load_extension("gh")
    end,
    event = 'CmdlineEnter',
  },
  {
    "cljoly/telescope-repo.nvim",
    dependencies = "nvim-telescope/telescope.nvim",
    config = function()
      require("telescope").load_extension("repo")
    end,
    event = 'CmdlineEnter',
  },
  {
    "luc-tielen/telescope_hoogle",
    dependencies = "nvim-telescope/telescope.nvim",
    config = function()
      require("telescope").load_extension("hoogle")
    end,
    event = 'CmdlineEnter',
  }
}
