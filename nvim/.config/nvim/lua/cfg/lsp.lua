local clangd_tools = require('clangd_extensions')
local haskell_tools = require('haskell-tools')
local lspconfig = require('lspconfig')
local cmp = require('cmp_nvim_lsp')

--create nvim-cmp capabilities for lsp client
local cap = cmp.default_capabilities(vim.lsp.protocol.make_client_capabilities())


-- local function format()
--   vim.lsp.buf.format { async = true }
-- end


-- callback function on lsp buffer attatch
  -- define keymaps for LSP buffers
local callback = function(_, bufnr)
  local opts = { noremap = true, silent = true, buffer = bufnr }
  local function list_ws_folders()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end

  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- gd: go to definition of sym under cursor
  vim.keymap.set('n', 'gd',         vim.lsp.buf.definition, opts)
  -- gD: go to declaration of sym under cursor
  vim.keymap.set('n', 'gD',         vim.lsp.buf.declaration, opts)

  -- gi: list all implementations for symbol under cursor in qf window
  vim.keymap.set('n', 'gi',         vim.lsp.buf.implementation, opts)
  -- gr: list all references to the symbol under cursor in qf window
  vim.keymap.set('n', 'gr',         vim.lsp.buf.references, opts)
  -- K: display hover info for symbol under cursor in floating window
  vim.keymap.set('n', 'K',          vim.lsp.buf.hover, opts)
  -- \h: display signature info for the symbol under curor in flaoting window
  vim.keymap.set('n', '<leader>h',  vim.lsp.buf.signature_help, opts)

  -- \d: jump to the definition of the type for the symbol under cursor
  vim.keymap.set('n', '<leader>d',  vim.lsp.buf.type_definition, opts)
  -- \rn: rename all buffer references to symbol under cursor
  vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
  -- \ca: use floating menu to perform code action
  vim.keymap.set('n', '<leader>ca', ':CodeActionMenu<CR>', opts)


  -- \wl: list all folders in current workspace
  vim.keymap.set('n', '<leader>wl', list_ws_folders ,opts)
  -- \wa: add new folder to workspace
  vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
  -- \wr: remove a wolder from workspace
  vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
  -- \fo: lsp format buffer
 -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format{async = true}
  end, { desc = 'Format current buffer with LSP' })
  -- vim.keymap.set('n', '<leader>fo', format, opts)
end

-- disable diagnostic inline virtual text and sign column, keep underline
  -- use trouble as interface to diagnostics
vim.diagnostic.config({virtual_text = false, signs = false})

local servers = {
  'awk_ls',
  'bashls',
  'cmake',
  'cssls',
  'html',
  'jsonls',
  'pyright',
  'r_language_server',
  -- 'texlab',
}

local conf = {
  on_attach = callback,
  settings = {
    capabilities = cap,
    telemetry = {enable = false},
  },
  single_file_support = true,
}


for _, lsp in pairs(servers) do
  lspconfig[lsp].setup(conf)
end


-- set up clangd lsp extensions
clangd_tools.setup{server = conf}

-- setup haskell tools
haskell_tools.setup{
  hls = conf,
  tools = {
    repl = {handler = 'toggleterm'}
  },
}

-- setup sumneko lua langauge server
lspconfig.sumneko_lua.setup {
  on_attatch = callback,
  settings = {
    Lua = {
      runtime = {version = 'LuaJIT'},
      diagnostics = {globals = {'vim'}},
      workspace = {library = vim.api.nvim_get_runtime_file("", true)},
    },
    capabilities = cap,
    telemetry = {enable = false},
  },
  single_file_support = true,
}
