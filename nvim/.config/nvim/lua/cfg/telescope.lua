local telescope = require('telescope')
local builtin = require('telescope.builtin')
local trouble = require('trouble.providers.telescope')
-- local previewers = require('telescope.previewers')


-- telescope setup function
telescope.setup{
  defaults = {
    -- initial_mode = 'normal',
    theme = 'dropdown',
    -- file_previewer = previewers.bat.new,
    mappings = {
      i = { ['<C-t>'] = trouble.open_with_trouble },
      n = { ['<C-t>'] = trouble.open_with_trouble },
    },
    pickers = {
      lsp_references = {
        theme = 'get_cursor',
      },
      lsp_definitions = {
        theme = 'get_cursor',
      },
      lsp_type_definitions = {
        theme = 'get_cursor',
      },
      lsp_implementations = {
        theme = 'get_cursor',
      },
      lsp_document_symbols = {
        theme = 'get_cursor',
      },
      lsp_workspace_symbols = {
        theme = 'get_cursor',
      },
      lsp_dynamic_workspace_symbols = {
        theme = 'get_cursor',
      },
      live_grep = {
        theme = 'ivy',
      },
      grep_string = {
        theme = 'ivy',
      },
      buffers = {
        theme = 'ivy',
      },
    },
  },
  extensions = {
    hoogle = {
      theme = 'ivy',
    },
  },
}

-- set wrap line mode for telescope preview
vim.api.nvim_create_autocmd('User', {
  pattern = 'TelescopePreviewerLoaded',
  callback = function() vim.cmd[[setlocal wrap]] end
})

-- auto load these extensions
telescope.load_extension('fzf')

-- telescope keymaps;


-- \f: search through files
vim.keymap.set('n', '<leader>f',   builtin.find_files)
-- \fd: search through fd
vim.keymap.set('n', '<leader>fd',  builtin.fd)
-- \ff: search through live g
vim.keymap.set('n', '<leader>ff',  builtin.live_grep, opts)

-- vim internal pickers

-- \fb: search through available buffers
vim.keymap.set('n', '<leader>fb',  builtin.buffers)
-- \fm: search through vim marks
vim.keymap.set('n', '<leader>fm',  builtin.marks)
-- \fr: search through vim registers
vim.keymap.set('n', '<leader>fr',  builtin.registers)
-- \fh: search through vim help tags
vim.keymap.set('n', '<leader>fht', builtin.help_tags)
-- \fmp: search through system man pages
vim.keymap.set('n', '<leader>fmp', builtin.man_pages)

-- git pickers

-- \fg: search through git files
vim.keymap.set('n', '<leader>fg',  builtin.git_files)
-- \fbc:search through git branch commits
vim.keymap.set('n', '<leader>fbc', builtin.git_bcommits)
-- \fgc: search through git commits
vim.keymap.set('n', '<leader>fgc', builtin.git_commits)
-- \fgb: search through git branches
vim.keymap.set('n', '<leader>fgb', builtin.git_branches)
-- \fgs: search through git stash files
vim.keymap.set('n', '<leader>fgs', builtin.git_stash)

-- nvim builtin lsp pickers

-- \fds: search through lsp document symbols
vim.keymap.set('n', '<leader>fds',  builtin.lsp_document_symbols)
-- \fws: search through lsp worksapces symbols
vim.keymap.set('n', '<leader>fws', builtin.lsp_workspace_symbols)
-- \flr: search through lsp references
vim.keymap.set('n', '<leader>flr', builtin.lsp_references)
