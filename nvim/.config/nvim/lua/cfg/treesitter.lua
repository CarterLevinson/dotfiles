-- Treesitter configuration
local ts_languages = {
  'bash',
  'c',
  'cpp',
  'cmake',
  'css',
  'cuda',
  'git_rebase',
  'gitattributes',
  'gitcommit',
  'gitignore',
  'haskell',
  'html',
  'java',
  'jq',
  'json',
  'json5',
  'latex',
  'ledger',
  'llvm',
  'lua',
  'make',
  'markdown',
  'ninja',
  'ocaml',
  'perl',
  'python',
  'query',
  'r',
  'regex',
  'rust',
  'sql',
  'verilog',
  'vim',
}

-- Parsers must be installed manually via :TSInstall
-- use git instead of curl to install parsers
require('nvim-treesitter.install').prefer_git = true

require('nvim-treesitter.configs').setup {
  -- table of languages to always install
  ensure_installed = ts_languages,
  -- Automatically install missing parsers when entering buffer
  auto_install = true,
  -- install parsers synchronously
  sync_install = false,
  -- configure how TS does highhlighting
  highlight = {
    -- false will disable the whole extension
    enable = true,
    disable = {},
    -- speed up the parsing of the document
    additional_vim_regex_highlighting = false,
  },
  -- incremental selection based on named nodes from the grammar
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'gnn',
      node_incremental = 'grn',
      scope_incremental = 'grc',
      node_decremental = 'grm',
    },
  },
  indent = {
    enable = false,
  },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
       -- 'f' for function
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        -- 'F' for function call
        ['aF'] = '@call.outer',
        ['iF'] = '@call.inner',
        -- 'c' for class
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",

        -- 'C' for  conditional or control
        ['aC'] = '@conditional.outer',
        ['iC'] = '@conditional.inner',
        -- 'a' for argument
        ['aa'] = '@parameter.outer',
        ['ia'] = '@parameter.inner',
        -- 'A' for attribute
        ['aA'] = '@attribute.outer',
        ['iA'] = '@attribute.inner',
        -- 'l' for loop
        ['aL'] = '@loop.outer',
        ['iL'] = '@loop.inner',
        -- 'L' (line??) for statement
        ['al'] = '@statement.outer',
        ['il'] = '@statement.inner',
        -- 'tf' for template function
        ['tf'] = '@function.outer.start',
        -- 'tc' for template class
        ['tc'] = '@class.outer.start',
      },
    },
    move = {
      enable = true,
      set_jumps = true,    -- whether to set jumps in the jumplist
      goto_next_start = {
        [']m'] = '@function.outer',
        [']]'] = '@class.outer',
      },
      goto_next_end = {
        [']M'] = '@function.outer',
        [']['] = '@class.outer',
      },
      goto_previous_start = {
        ['[m'] = '@function.outer',
        ['[['] = '@class.outer',
      },
      goto_previous_end = {
        ['[M'] = '@function.outer',
        ['[]'] = '@class.outer',
      },
    },
    swap = {
      enable = true,
      swap_next = {
        ['<leader>a'] = '@parameter.inner',
      },
      swap_previous = {
        ['<leader>A'] = '@parameter.inner',
      },
    },
  },
  refactor = {
    smart_rename = {
      enable = true,
      keymaps = {
        smart_rename = 'grr',
      },
      navigation = {
        enable = true,
        keymaps = {
          list_definitions = 'glD',
          list_definition_toc = 'g0',
          goto_next_usage = '<a-]>',
          goto_previous_usage = '<a-[>',
       },
     },
    },
  },
  playground = {
    enable = true,
  },
  query_linter = {
    enable = true,
    use_virtual_text = true,
    line_events = {'BufWrite', 'CurshorHold'},
  },
}
