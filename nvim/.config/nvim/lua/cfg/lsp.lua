local clangd_tools = require('clangd_extensions')
local haskell_tools = require('haskell-tools')
local rust_tools = require('rust-tools')
local lspconfig = require('lspconfig')
local cmp = require('cmp_nvim_lsp')
local map = require('utils.map')

--create nvim-cmp capabilities for lsp client
local cap = cmp.default_capabilities(vim.lsp.protocol.make_client_capabilities())

-- disable diagnostic inline virtual text and sign column, keep underline
  -- use trouble as interface to diagnostics
vim.diagnostic.config({virtual_text = false, signs = false})

-- callback function on lsp buffer attatch
  -- define keymaps for LSP buffers
local function callback(_, bufnr)
  local opts = { noremap = true, silent = true, buffer = bufnr }

  local function list_ws_folders()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end

  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- gd: go to definition of sym under cursor
  nmap('gd', vim.lsp.buf.definition, opts)
  -- gD: go to declaration of sym under cursor
  nmap('gD', vim.lsp.buf.declaration, opts)

  -- gi: list all implementations for symbol under cursor in qf window
  nmap('gi', vim.lsp.buf.implementation, opts)
  -- gr: list all references to the symbol under cursor in qf window
  nmap('gr', vim.lsp.buf.references, opts)
  -- K: display hover info for symbol under cursor in floating window
  nmap('K', vim.lsp.buf.hover, opts)
  -- \h: display signature info for the symbol under curor in flaoting window
  nmap('<leader>h', vim.lsp.buf.signature_help, opts)

  -- \d: jump to the definition of the type for the symbol under cursor
  nmap('<leader>d', vim.lsp.buf.type_definition, opts)
  -- \rn: rename all buffer references to symbol under cursor
  nmap('<leader>rn', vim.lsp.buf.rename, opts)
  -- \ca: use floating menu to perform code action
  nmap('<leader>ca', ':CodeActionMenu<CR>', opts)

  -- \wl: list all folders in current workspace
  nmap('<leader>wl', list_ws_folders, opts)
  -- \wa: add new folder to workspace
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
  -- \wr: remove a wolder from workspace
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)

 -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format{async = true}
  end, { desc = 'Format current buffer with LSP' })
end

local conf = {
  on_attach = callback,
  settings = {
    capabilities = cap,
    telemetry = {enable = false},
  },
  single_file_support = true,
}

local lua_conf = {
  on_attach = callback,
  settings = {
    Lua = {
      runtime = {
        version = 'LuaJIT'
      },
      diagnostics = {
        globals = {'vim'}
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true)
      },
      capabilities = cap,
      telemetry = {enable = false},
    }
  }
}

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

for _, lsp in pairs(servers) do
  lspconfig[lsp].setup(conf)
end

-- setup lua server
lspconfig.sumneko_lua.setup(lua_conf)

-- set up clangd lsp extensions
clangd_tools.setup{server = conf}

--setup rust analyzer
rust_tools.setup{server = conf}

-- setup haskell tools
haskell_tools.setup{
  hls = conf,
  tools = {
    repl = {
      handler = 'toggleterm'
    }
  },
}

lspconfig.sumneko_lua.setup{
  on_attach = callback,
  settings = {
    Lua = {
      runtime = {
        version = 'LuaJIT'
      },
      diagnostics = {
        globals = {
          'vim'
        }
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true)
      },
    },
    capabilities = cap,
    telemetry = {enable = false},
  },
  single_file_support = true,
}
