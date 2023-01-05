-- only go through this file if we have packer
local status_ok, packer = pcall(require, 'packer')
if not status_ok then
  return
end

-- only required if you have packer configured as `opt`
-- vim.cmd [[packadd packer.nvim]]

-- automatically run :PackerCompile whenever plugins.lua is updated
vim.api.nvim_create_autocmd('BufWritePost', {
    group = vim.api.nvim_create_augroup('PACKER', { clear = true }),
    pattern = 'plugins.lua',
    command = 'source <afile> | PackerCompile',
})

local packer_conf = {
  -- profile all plugins with load time above threshold 0ms
  profile = {
    enable = true,
    threshold = 0,
  },
  -- use a floating window instead of vsplit for packer cmds
  display = {
    open_fn = function()
      return require'packer.util'.float {border = 'rounded'}
    end,
  },
}

-- packer startup function
return packer.startup({
  function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- Speed up loading Lua modules in Neovim to improve startup time. Source
    -- this plugin as soon as possible in init.lua
    -- Note: This optimization breaks the loading order guarantee of the paths in
    -- 'runtimepath'. If you rely on this ordering then you can disable this
    -- cache using:
    --
    -- _G.__luacache_config = { modpaths = { enable = false } }.
    use 'lewis6991/impatient.nvim'

    use 'nathom/filetype.nvim'

    -- LSP plugins
    use {
      {
        'neovim/nvim-lspconfig', -- configuration plugin for neovim builtin lsp
        requires = {
          'hrsh7th/nvim-cmp',
          'p00f/clangd_extensions.nvim',
          'MrcJkb/haskell-tools.nvim',
          'akinsho/toggleterm.nvim',
        },
        config = [[require'cfg.lsp']],
      },
      {
        'kosayoda/nvim-lightbulb', -- add vscode lightbulb icon for code actions
        requires = 'antoinemadec/FixCursorHold.nvim',
        config = [[require'cfg.lightbulb']],
        after = 'nvim-lspconfig', -- lazy load
      },
      {
        'weilbith/nvim-code-action-menu', -- floating menu interface for ca's
        cmd = 'CodeActionMenu',
        after = 'nvim-lspconfig', -- lazy load
      },
    }

    -- trouble.nvim: better diagnositics, quickfix and loclist window
    use {
      'folke/trouble.nvim',  -- better handling of diagnostics and quickfix
      requires = 'kyazdani42/nvim-web-devicons',
      config = [[require'cfg.trouble']],
    }

    -- DAP plugins
    use {
      {
        'mfussenegger/nvim-dap',
        config = [[require'cfg.dap']],
      },
      {
        'rcarriga/nvim-dap-ui',
        requires = 'mfussenegger/nvim-dap',
        config = [[require'cfg.dap-ui']],
        after = 'nvim-dap',
      },
    }

    -- Treesitter plugins
    use {
      {
        'nvim-treesitter/nvim-treesitter', -- treesitter parser / syntax highlight
        config = [[require'cfg.treesitter']],
        run = ':TSUpdate',
        event = 'BufWinEnter', -- lazy load
      },
      {
        'nvim-treesitter/nvim-treesitter-textobjects', -- ts semantic motions
        after = 'nvim-treesitter',
      },
      {
        'nvim-treesitter/nvim-treesitter-refactor', -- ts based refactor
        after = 'nvim-treesitter',
      },
      {
        'nvim-treesitter/nvim-treesitter-context', -- display ts "context"
        requires = 'nvim-treesitter',
        config = [[require'cfg.treesitter-context']],
        after = 'nvim-treesitter',
      },
      {
        'nvim-treesitter/playground', -- experiment with treesitter queries
        cmd = 'TSPlaygroundToggle',
        after = 'nvim-treesitter'
      },
    }

    -- cmp: async modular neovim completion written in lua
    use {
      'hrsh7th/nvim-cmp',
      -- various nvim-cmp source plugins
      requires = {
        'dcampos/nvim-snippy', -- TextMate style snippets plugin
        'dcampos/cmp-snippy',
        'hrsh7th/cmp-calc', -- sources from plugin author
        'hrsh7th/cmp-path',
        'hrsh7th/cmp-buffer',
        'hrsh7th/cmp-cmdline',
        'hrsh7th/cmp-nvim-lua',
        'hrsh7th/cmp-nvim-lsp',
        'hrsh7th/cmp-nvim-lsp-signature-help',
        'hrsh7th/cmp-nvim-lsp-document-symbol',
        'ray-x/cmp-treesitter', -- other useful sources
        'kristijanhusak/vim-dadbod-completion',
        'petertriho/cmp-git',
        'amarakon/nvim-cmp-lua-latex-symbols',
        'lukas-reineke/cmp-rg',
        'paopaol/cmp-doxygen',
        'rcarriga/cmp-dap',
        'onsails/lspkind.nvim', -- LSP icons
      },
      config = [[require'cfg.cmp']],
    }

    -- telescope: neovim's builtin fuzzy finder
    use {
      {
        'nvim-telescope/telescope.nvim',
        branch = '0.1.x',
        requires = {
          'nvim-lua/popup.nvim', -- telescope dependencies
          'nvim-lua/plenary.nvim',
          'nvim-telescope/telescope-fzf-native.nvim', -- telescope extensions
          'nvim-telescope/telescope-packer.nvim',
          'nvim-telescope/telescope-github.nvim',
          'luc-tielen/telescope_hoogle', -- telescope hoogle integration!
          'cljoly/telescope-repo.nvim',
        },
        wants ={
          'popup.nvim',
          'plenary.nvim',
          'telescope-fzf-native.nvim',
        },
        config = [[require'cfg.telescope']],
      },
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        run  = 'make',
      },
      {
        'cljoly/telescope-repo.nvim',
        config = [[require'cfg.telescope-repo']],
        event = 'CmdlineEnter',
      },
      {
        'nvim-telescope/telescope-packer.nvim',
        config = [[require'cfg.telescope-packer']],
        event = 'CmdlineEnter',
      },
      {
        'nvim-telescope/telescope-github.nvim',
        config = [[require'cfg.telescope-github']],
        event = 'CmdlineEnter',
      },
      {
        'luc-tielen/telescope_hoogle',
        config = [[require'cfg.telescope-hoogle']],
        event = 'CmdlineEnter',
      },
    }

    -- template plugins
    use {
      'danymat/neogen',
      requires = "nvim-treesitter/nvim-treesitter",
      config = [[require'cfg.neogen']],
    }

    -- status line plugins
    use {
      {
        'nvim-lualine/lualine.nvim', -- status line written in lua
        requires = 'kyazdani42/nvim-web-devicons',
        config = [[require'cfg.lualine']],
        after = 'tabline.nvim',
      },
      {
        'kdheepak/tabline.nvim', -- tab line written in lua
        requires = 'kyazdani42/nvim-web-devicons',
        config = [[require'cfg.tabline']],
        event = 'BufWinEnter',
      },
      { 'nvim-lua/lsp-status.nvim', disable = true },
    }

    -- vista.vim: tagbar like plugin with added support for LSP symbols
    use {
      'liuchengxu/vista.vim',
      config = [[require'cfg.vista']],
      cmd = 'Vista',
      keys = {
        '<leader>v',   -- toggle vista window
        '<leader>vv',  -- focus vista window
        '<leader>vt',  -- open vista ctags
        '<leader>vl',  -- open vista lsp
        '<leader>vs',  -- show vista
        '<leader>vf',  -- open vista finder
      },
    }

    -- toggleterm.nvim: builtin neovim terminal wrapper
     use {
      'akinsho/toggleterm.nvim',
      config = [[require'cfg.toggleterm']],
      tag = 'v2.*',
    }

    use {
      'elihunter173/dirbuf.nvim',
      config = [[require'cfg.dirbuf']],
    }

    -- vim database integration
    use {
      'kristijanhusak/vim-dadbod-ui',
      requires = 'tpope/vim-dadbod',
      cmd = { 'DB', 'DBUI' },
    }

    -- use rsync or sftp to sync files in a remote/local project
    use {
      'superevilmegaco/AutoRemoteSync.nvim',
      config = [[require'cfg.auto-remote-sync']],
    }

    -- interface / plugin framework for interacting with remote resources
    use {
      'miversen33/netman.nvim',
      config = [[require'netman']],
      opt = true
    }

    -- async build tools
    use {
      'radenling/vim-dispatch-neovim',
      requires = 'tpope/vim-dispatch',
      config = [[require'cfg.dispatch']],
    }

    -- marks enhancement features
    use {
      'chentoast/marks.nvim',
      event = 'BufWinEnter',
      config = [[require'cfg.marks']],
    }

    -- register utility / display
    use {
      'tversteeg/registers.nvim',
      event = 'BufWinEnter',
      config = [[require'cfg.registers']],
    }

    -- shell wrapper commands for vim
    use 'tpope/vim-eunuch'
    -- add some useful unicode commands
    use 'chrisbra/unicode.vim'

    -- improve native vim ui-select
    use { 'stevearc/dressing.nvim', config = [[require'cfg.dressing']] }
    -- auto switch neovim directory to project root
    use { 'notjedi/nvim-rooter.lua', config = [[require'cfg.rooter']] }
    -- undotree: provide graphic representation of vim's undo tree
    use { 'mbbill/undotree',  cmd = 'UndotreeToggle' }

    -- editing plugins
    --use { 'windwp/nvim-autopairs', config = [[require'cfg.auto-pairs']], }
    use {
      { 'numToStr/Comment.nvim', config = [[require'cfg.comment']] },
      { 'kylechui/nvim-surround', config = [[require'cfg.surround']] },
      { 'junegunn/vim-easy-align', config = [[require'cfg.easy-align']] },
      { 'ntpeters/vim-better-whitespace', config = [[require'cfg.whitespace']] }
    }


    -- git plugins

    use {
      { 'tpope/vim-fugitive' }, -- git wrapper functions
      {
        'lewis6991/gitsigns.nvim', -- display git diff signs in columns
        tag = 'release',
        requires = 'kyazdani42/nvim-web-devicons',
        config = [[require'cfg.gitsigns']],
        event = 'BufWinEnter',
      },
      {
        'pwntester/octo.nvim', -- github cli wrappers
        requires = {
          'nvim-lua/plenary.nvim',
          'nvim-telescope/telescope.nvim',
          'kyazdani42/nvim-web-devicons',
        },
        config = [[require'cfg.octo']],
        cmd = "Octo",
      }
    }

    -- filetype plugins:

    -- use compiler plugins for linters?

    -- C/C++
    use { 'vim-scripts/cscope.vim', ft = {'c', 'cpp'} }
    use { 'vim-scripts/cpp_cppcheck.vim', ft = {'c', 'cpp'} }

    -- haskell
    use { 'vmchale/pointfree', ft = {'haskell', 'lhaskell'} }
    use { 'mpickering/hlint-refactor-vim', ft = { 'haskell', 'lhaskell' } }
    use { 'neovimhaskell/haskell-vim', ft = { 'haskell', 'lhaskell'} }

    -- LaTex
    use { 'lervag/vimtex', config = [[require'cfg.vimtex']]  }

    -- gives commands to preview markdown in the browser
    use {
      'iamcco/markdown-preview.nvim',
      config = [[require'cfg.markdown-preview']],
      run = function() vim.fn["mkdp#util#install"]() end,
      ft = {'markdown', 'Rmd'},
      opt = true,
    }

    use {
      'ellisonleao/glow.nvim',
      config = [[require'cfg.glow']],
      ft = { 'markdown', 'Rmd' },
      disable = true,
    }
    -- follow markdown links with enter
    use { 'jghauser/follow-md-links.nvim', ft = {'markdown', 'Rmd'} }

    -- Tabular Data
    use { 'chrisbra/csv.vim', ft = {'csv', 'tsv'}, opt = true }

    -- aesthetic plugins
    use { 'karb94/neoscroll.nvim', config = [[require'cfg.scroll']] }
    use { 'xiyaowong/nvim-transparent', config = [[require'cfg.transparent']] }

    --use { 'goolord/alpha-nvim',config = [[require'cfg.alpha']], opt = true }

    -- colorscheme plugins
    use {
      'tanvirtin/monokai.nvim',
      'rafamadriz/neon',
      'sainnhe/sonokai',
      'projekt0n/github-nvim-theme',
      'kvrohit/substrata.nvim',
      'bignimbus/pop-punk.vim',
    }

    -- misc plugins

    use {
      'soywod/himalaya',
      config = [[require'cfg.himalaya']],
      rtp =  'vim',
      disable = true,
    }

    -- mbsyncrc syntax file
    use { 'Fymyte/mbsync.vim', ft = 'mbsync' }

    -- kitty terminal plugins
    use {
      -- syntax highlighting for kitty config files
      { 'fladson/vim-kitty', opt = true },
      -- tmux like navigation with kitty windows
      { 'knubie/vim-kitty-navigator', run = 'cp ./*.py ~/.config/kitty', disable = true },
      -- use kitty graphics protocol to display images in nvim buffer
      { 'edluffy/hologram.nvim', disable = true },
    }


    -- Tradingview 'pinescript' syntax highlighting
    -- use { 'jbmorgado/vim-pine-script', ft = 'psl' }

    -- kmonad config file syntax highlighting
    -- use { 'kmonad/kmonad-vim', ft = 'kbd' }
end,
config = packer_conf
})
