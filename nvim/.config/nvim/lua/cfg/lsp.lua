local clangd_exts = require('clangd_extensions')
local haskell_tools = require('haskell-tools')
local lspconfig = require('lspconfig')
local cmp = require('cmp_nvim_lsp')

--create nvim-cmp capabilities for lsp client
local cap = cmp.default_capabilities(vim.lsp.protocol.make_client_capabilities())

local function list_ws_folders()
  print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
end

local function format()
  vim.lsp.buf.format { async = true }
end

local open_menu = [[<CMD>CodeActionMenu<CR>]]

-- local menu = [[<CMD>lua require'code_action_menu'.open_code_action_menu()<CR>]]

-- callback function on lsp buffer attatch
  -- define keymaps for LSP buffers
local callback = function(_, bufn)
  local opts = { noremap = true, silent = true, buffer = bufn }
  vim.api.nvim_buf_set_option(bufn, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

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
  vim.keymap.set('n', '<leader>ca', open_menu, opts)

  -- \wl: list all folders in current workspace
  vim.keymap.set('n', '<leader>wl', list_ws_folders ,opts)
  -- \wa: add new folder to workspace
  vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
  -- \wr: remove a wolder from workspace
  vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
  -- \fo: lsp format buffer
  vim.keymap.set('n', '<leader>fo', format, opts)
  -- haskell tools specific
  -- vim.keymap.set('n', '<space>ca',  vim.lsp.codelens.run, opts)
  -- vim.keymap.set('n', '<leader>hs', haskell_tools.hoogle.hoogle_signature, opts)
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
  'texlab',
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

-- set up clangd lsp extensions
clangd_exts.setup{server = conf}

-- add command alias A for alternate
vim.cmd[[cnoreabbrev A ClangdSwitchSourceHeader]]

-- setup haskell tools
haskell_tools.setup{
  hls = conf,
  tools = {
    repl = {handler = 'toggleterm'}
  },
}


-- haskell keybindings
local function bufferGHCi()
  haskell_tools.repl.toggle(vim.api.nvim_buf_get_name(0))
end

-- Toggle a GHCi repl for the current package
vim.keymap.set('n', '<leader>rr', haskell_tools.repl.toggle)
-- Toggle a GHCi repl for the current buffer
vim.keymap.set('n', '<leader>rf', bufferGHCi)
-- close a GHCi repl
vim.keymap.set('n', '<leader>rq', haskell_tools.repl.quit)
