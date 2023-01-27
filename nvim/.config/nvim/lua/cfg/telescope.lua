local telescope = require('telescope')
local builtin = require('telescope.builtin')

-- telescope setup function
telescope.setup{
  pickers = {
    find_files = {
      theme = "ivy"
    },
    live_grep = {
      theme = "ivy"
    },
    buffers = {
      theme = "ivy"
    },
    lsp_document_symbols = {
      theme = "ivy"
    },
    lsp_workspace_symbols = {
      theme = "ivy"
    }
  }
}

-- set wrap line mode for telescope preview
vim.api.nvim_create_autocmd('User', {
  pattern = 'TelescopePreviewerLoaded',
  callback = function() vim.cmd[[setlocal wrap]] end
})

-- telescope keymaps;

-- \f: search through files
nmap('<leader>f', builtin.find_files)
-- \ff: search through live rg
nmap('<leader>ff', builtin.live_grep)

-- \fd: search through fd
nmap('<leader>fd', builtin.fd)
-- \fb: search through available buffers
nmap('<leader>fb', builtin.buffers)

-- \fm: search through vim marks
nmap('<leader>fm', builtin.marks)
-- \fr: search through vim registers
nmap('<leader>fr', builtin.registers)

-- \fh: search through vim help tags
nmap('<leader>fht', builtin.help_tags)
-- \fmp: search through system man pages
nmap('<leader>fmp', builtin.man_pages)

-- \fg: search through git files
nmap('<leader>fg', builtin.git_files)
-- \fgb: search through git branches
nmap('<leader>fgb', builtin.git_branches)
-- \fgc: search through git commits
nmap('<leader>fgc', builtin.git_commits)
-- \fbc:search through git branch commits
nmap('<leader>fbc', builtin.git_bcommits)
-- \fgs: search through git stash files
nmap('<leader>fgs', builtin.git_stash)

-- \fds: search through lsp document symbols
nmap('<leader>fds', builtin.lsp_document_symbols)
-- \fws: search through lsp worksapces symbols
nmap('<leader>fws', builtin.lsp_workspace_symbols)
-- \flr: search through lsp references
nmap('<leader>flr', builtin.lsp_references)
