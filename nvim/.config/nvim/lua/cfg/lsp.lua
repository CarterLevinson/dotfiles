local clangd_tools = require("clangd_extensions")
local haskell_tools = require("haskell-tools")
local rust_tools = require("rust-tools")
local neodev = require("neodev")

local goto_preview = require('goto-preview')
local cmp = require("cmp_nvim_lsp")

local lspconfig = require("lspconfig")
local lsp = vim.lsp

-- disable diagnostic inline virtual text and sign column, keep underline
-- use trouble as interface to diagnostics
vim.diagnostic.config({ virtual_text = false, signs = false })

--create nvim-cmp capabilities for lsp client
local cap = cmp.default_capabilities(lsp.protocol.make_client_capabilities())

goto_preview.setup {}
-- callback function on lsp buffer attatch
-- define keymaps for LSP buffers
local function callback(_, bufnr)
  local opts = { noremap = true, silent = true, buffer = bufnr }
  Nmap("gd", vim.lsp.buf.definition, opts) -- go to definition
  Nmap("gD", vim.lsp.buf.declaration, opts) -- go to declaration

  Nmap("gi", vim.lsp.buf.implementation, opts) -- list imps in qf
  Nmap("gr", vim.lsp.buf.references, opts) -- list all refs in qf
  Nmap("K", vim.lsp.buf.hover, opts)
  Nmap("<C-k>", vim.lsp.buf.signature_help, opts)
  Nmap("<leader>d", vim.lsp.buf.type_definition, opts)
  -- Nmap("<leader>rn", vim.lsp.buf.rename, opts)
  Nmap("<leader>rn", ":IncRename ", opts)
  Nmap("<leader>ca", Cmd "CodeActionMenu", opts)
  -- Nmap("<leader>ca", vim.lsp.buf.code_action, opts)
  Nmap("gpd", goto_preview.goto_preview_definition, opts)
  Nmap("gpt", goto_preview.goto_preview_type_definition, opts)
  Nmap("gpr", goto_preview.goto_preview_references, opts)
  Nmap("gpc", goto_preview.close_all_win, opts)

  Nmap("<leader>wa", vim.lsp.buf.add_workspace_folder, opts)
  Nmap("<leader>wr", vim.lsp.buf.remove_workspace_folder, opts)

  Nmap("<leader>wl", Cmd "ListWS", opts)
  vim.api.nvim_buf_create_user_command(bufnr, "ListWS",
    function(_) print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end,
    { desc = "Print all folders in LSP workspace" }
  )

  Nmap("<leader>fo", Cmd "Format", opts)
  vim.api.nvim_buf_create_user_command(bufnr, "Format",
    function(_) vim.lsp.buf.format { async = true } end,
    { desc = "Format current buffer with LSP" }
  )

end

local conf = {
  on_attach = callback,
  settings = {
    capabilities = cap,
    telemetry = { enable = false },
  },
  single_file_support = true,
}

local sumneko = {
  Lua = {
    completion = { callSnippet = "Replace" },
    diagnostics = { globals = { "vim" } },
  }
}

local servers = {
  awk_ls = {},
  bashls = {},
  cmake = {},
  cssls = {},
  html = {},
  jsonls = {},
  pyright = {},
  r_language_server = {},
  texlab = {},
  sumneko_lua = sumneko,
}

-- setup neodev
neodev.setup {}

-- setup lsp servers
for lsp, settings in pairs(servers) do
  local options = conf
  if not (next(settings) == nil) then
    options.settings = vim.tbl_extend("force", options.settings, settings)
  end
  lspconfig[lsp].setup(options)
end


-- set up clangd lsp extensions
clangd_tools.setup { server = conf }

--setup rust analyzer
rust_tools.setup { server = conf }

-- setup haskell tools
haskell_tools.setup {
  hls = conf,
  tools = {
    repl = { handler = "toggleterm" }
  },
}
