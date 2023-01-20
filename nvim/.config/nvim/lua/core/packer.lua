-- only go through this file if we have packer
local status_ok, packer = pcall(require, 'packer')
if not status_ok then
  return
end

-- only required if you have packer configured as `opt`
-- vim.cmd [[packadd packer.nvim]]

-- automatically run :PackerCompile whenever plugins.lua is updated
-- vim.api.nvim_create_autocmd('BufWritePost', {
    -- group = vim.api.nvim_create_augroup('PACKER', { clear = true }),
    -- pattern = 'plugins.lua',
    -- command = 'source <afile> | PackerCompile',
-- })

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

    -- LSP plugins
    use {
      { 'neovim/nvim-lspconfig', -- configuration plugin for neovim builtin lsp
        requires = {
          'hrsh7th/nvim-cmp',
          'akinsho/toggleterm.nvim',
          'p00f/clangd_extensions.nvim',
          'simrat39/rust-tools.nvim',
          'MrcJkb/haskell-tools.nvim',
          'nvim-lua/plenary.nvim',
        },
        config = [[require'cfg.lsp']],
      },
      { 'kosayoda/nvim-lightbulb', -- add vscode lightbulb icon for code actions
        requires = 'antoinemadec/FixCursorHold.nvim',
        config = [[require'cfg.lightbulb']],
        after = 'nvim-lspconfig', -- lazy load
      },
      { 'weilbith/nvim-code-action-menu', -- floating menu interface for ca's
        cmd = 'CodeActionMenu',
        after = 'nvim-lspconfig', -- lazy load
      },
      -- { 'rmagatti/goto-preview',
      --   config = function() require('goto-preview').setup{} end,
      -- },
      { 'smjonas/inc-rename.nvim', config = [[require'cfg.inc-rename']] },
    }

    -- DAP plugins
    use {
      { 'rcarriga/nvim-dap-ui',
        requires = 'nvim-dap',
        after = 'nvim-dap',
        config = function() require('dapui').setup{} end,
      },
      { 'mfussenegger/nvim-dap', config = [[require'cfg.dap']] },
    }

    -- Treesitter plugins
    use {
      { 'nvim-treesitter/nvim-treesitter', -- treesitter parser / syntax highlight
        config = [[require'cfg.treesitter']],
        run = ':TSUpdate',
        event = 'BufWinEnter', -- lazy load
      },
      { 'nvim-treesitter/nvim-treesitter-textobjects', -- ts semantic motions
        after = 'nvim-treesitter',
      },
      { 'nvim-treesitter/nvim-treesitter-refactor', -- ts based refactor
        after = 'nvim-treesitter',
      },
      { 'nvim-treesitter/playground', -- experiment with treesitter queries
        after = 'nvim-treesitter',
        cmd = 'TSPlaygroundToggle',
      },
      { 'nvim-treesitter/nvim-treesitter-context', -- display ts "context"
        requires = 'nvim-treesitter',
        after = 'nvim-treesitter',
        config = function() require('treesitter-context').setup{} end,
      },
    }

    -- cmp: async modular neovim completion written in lua
    use {
      'hrsh7th/nvim-cmp',
      requires = {
        'dcampos/nvim-snippy', -- TextMate style snippets plugin
        'dcampos/cmp-snippy', -- various nvim-cmp source plugins:
        'hrsh7th/cmp-omni',
        'hrsh7th/cmp-buffer',
        'hrsh7th/cmp-path',
        'hrsh7th/cmp-calc',
        'hrsh7th/cmp-cmdline',
        'hrsh7th/cmp-nvim-lua',
        'hrsh7th/cmp-nvim-lsp',
        'hrsh7th/cmp-nvim-lsp-signature-help',
        'hrsh7th/cmp-nvim-lsp-document-symbol',
        'rcarriga/cmp-dap',
        'lukas-reineke/cmp-rg',
        'petertriho/cmp-git',
        'kristijanhusak/vim-dadbod-completion',
        'amarakon/nvim-cmp-lua-latex-symbols',
        'lukas-reineke/cmp-under-comparator',
        'onsails/lspkind.nvim', -- LSP icons
        -- 'ray-x/cmp-treesitter',
      },
      config = [[require'cfg.cmp']],
    }

    use {
      'ibhagwan/fzf-lua',
      requires = 'kyazdani42/nvim-web-devicons',
      config = [[require'cfg.fzf']],
    }

    use {
      'folke/trouble.nvim', -- better diagnositic, quickfix and loclist view
      requires = 'kyazdani42/nvim-web-devicons',
      config = [[require'cfg.trouble']],
    }
    --use {
      --{ 'kevinhwang91/nvim-bqf', ft = 'qf' },
      --{ 'onsails/diaglist.nvim', config = [[require'cfg.diaglist']] },
    --}

    -- template plugins
    use {
      'danymat/neogen',
      requires = "nvim-treesitter/nvim-treesitter",
      config = [[require'cfg.neogen']],
      event = 'CmdlineEnter',
    }

    -- status line plugins
    use {
      { 'nvim-lualine/lualine.nvim', -- statusline written in lua
        requires = 'kyazdani42/nvim-web-devicons',
        config = [[require'cfg.lualine']],
        event = 'BufWinEnter',
      },
      { 'kdheepak/tabline.nvim', -- tabline written in lua
        requires = 'kyazdani42/nvim-web-devicons',
        config = [[require'cfg.tabline']],
        event = 'BufWinEnter',
      },
    }

    -- hmmm
    --use  { 'tjdevries/express_line.nvim', disable = true }


    -- shell wrapper commands for vim
    use 'tpope/vim-eunuch'
    -- add some useful unicode commands
    use 'chrisbra/unicode.vim'
    -- undotree: provide graphic representation of vim's undo tree
    use 'mbbill/undotree'
    -- auto follow symlinks
    use 'aymericbeaumet/vim-symlink'
    -- auto create directories on file save
    use 'jghauser/mkdir.nvim'

    -- edit directories like vim buffers
    use {
      'elihunter173/dirbuf.nvim',
      config = function() require('dirbuf').setup{} end,
    }

    -- marks enhancement features
    use {
      'chentoast/marks.nvim',
      event = 'BufWinEnter',
      config = function() require('marks').setup{} end,
    }

    -- register utility / display
    use {
      'tversteeg/registers.nvim',
      event = 'BufWinEnter',
      config = function() require('registers').setup{} end,
    }

    -- comment toggling and more
    use {
      'numToStr/Comment.nvim',
      config = function() require('Comment').setup{} end,
    }

    -- motions for surroundings
    use {
      'kylechui/nvim-surround',
      config = function() require('nvim-surround').setup{} end,
    }

    -- interface / plugin framework for interacting with remote resources
    use {
      'miversen33/netman.nvim',
      config = function() require('netman') end,
      opt = true
    }

    -- trim and mark whitespace
    use {
      'johnfrankmorgan/whitespace.nvim',
      config = [[require'cfg.whitespace']],
    }


    -- easy alignment commands and keymaps
    use {
      'junegunn/vim-easy-align',
      config = [[require'cfg.easy-align']],
    }

    -- vista.vim: tagbar like plugin with added support for LSP symbols
    use {
      'liuchengxu/vista.vim',
      config = [[require'cfg.vista']],
    }

    -- toggleterm.nvim: builtin neovim terminal wrapper
     use {
      'akinsho/toggleterm.nvim',
      config = [[require'cfg.toggleterm']],
      tag = 'v2.*',
    }

    -- async build tools and task runner
    use {
      'radenling/vim-dispatch-neovim',
      requires = 'tpope/vim-dispatch',
      config = [[require'cfg.dispatch']],
      event = 'CmdlineEnter',
    }

    -- vim database integration
    use {
      'kristijanhusak/vim-dadbod-ui',
      requires = 'tpope/vim-dadbod',
      cmd = {'DB', 'DBUI'},
    }


    -- use rsync or sftp to sync files in a remote/local project
    use {
      'superevilmegaco/AutoRemoteSync.nvim',
      config = [[require'cfg.sync']],
    }

    -- auto switch neovim directory to project root
    use {
      'notjedi/nvim-rooter.lua',
      config = [[require'cfg.rooter']]
    }

    -- neovim session management
    use {
      'dhruvasagar/vim-prosession',
      requires = 'tpope/vim-obsession',
      config = [[require'cfg.prosession']],
      disable = true,
    }

    -- git plugins
    use 'tpope/vim-fugitive'  -- git wrapper functions

    use {
      'lewis6991/gitsigns.nvim', -- display git diff signs in columns
      -- tag = 'release',
      requires = 'kyazdani42/nvim-web-devicons',
      config = function() require('gitsigns').setup{} end,
      event = 'BufWinEnter',
    }

    --use {
      --'pwntester/octo.nvim', -- github cli wrappers
      --requires = {
        --'nvim-lua/plenary.nvim',
        --'nvim-telescope/telescope.nvim',
        --'kyazdani42/nvim-web-devicons',
      --},
      --config = [[require'cfg.octo']],
      --cmd = "Octo",
      --disable = true,
    --}

    --use {
      --'ldelossa/gh.nvim',
      --requires = 'ldelossa/litee.nvim',
      --config = [[require'cfg.gh']],
      --disable = true,
    --}


    -- filetype plugins:

    -- use compiler plugins for linters?

    -- for noevim lua development
    use 'folke/neodev.nvim'

    -- for tabular data files
    use 'chrisbra/csv.vim'

    -- C/C++
    use 'vim-scripts/cscope.vim'
    use 'vim-scripts/cpp_cppcheck.vim'
    use 'vim-scripts/a.vim'

    -- haskell
    use 'mpickering/hlint-refactor-vim'
    use 'neovimhaskell/haskell-vim'
    use 'vmchale/pointfree'
    -- use 'Twinside/vim-hoogle'

    -- LaTex
    use {
      'lervag/vimtex',
      config = [[require'cfg.vimtex']]
    }

    -- Markdown
    use {
      'iamcco/markdown-preview.nvim',
      config = [[require'cfg.md-preview']],
      run = function() vim.fn["mkdp#util#install"]() end,
      ft = {'md', 'Rmd'},
    }

    -- follow markdown links with enter
    use {
      'jghauser/follow-md-links.nvim',
      ft = {'md', 'Rmd'}
    }

    -- animation / window based plugins
    use {
      { 'karb94/neoscroll.nvim',
        config = [[require'cfg.scroll']]
      },
      { 'xiyaowong/nvim-transparent',
        config = [[require'cfg.transparent']]
      },
      { 'anuvyklack/windows.nvim',
        requires = {
          'anuvyklack/middleclass',
          'anuvyklack/animation.nvim',
        },
        config = [[require'cfg.windows']],
        disable = true,
      },
    }

    -- colorscheme plugins
    use {
      'tanvirtin/monokai.nvim',
      'sainnhe/sonokai',
      'projekt0n/github-nvim-theme',
      'folke/tokyonight.nvim',
    }

    -- syntax plugins
    use {
      'mboughaba/i3config.vim',
      'Fymyte/mbsync.vim',
      'kmonad/kmonad-vim',
      'jbmorgado/vim-pine-script',
    }

    -- disabled plugins

    --use { 'fladson/vim-kitty', disable = true }

    use {
      'goolord/alpha-nvim',
      config = [[require'cfg.alpha']],
      disable = true
    }

    use {
      'soywod/himalaya',
      config = [[require'cfg.himalaya']],
      rtp =  'vim',
      disable = true,
    }
  end,
config = packer_conf
})
