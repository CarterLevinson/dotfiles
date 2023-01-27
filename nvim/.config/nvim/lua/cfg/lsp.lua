local clangd_tools = require("clangd_extensions")
local haskell_tools = require("haskell-tools")
local neodev = require("neodev")
neodev.setup {}

local goto_preview = require('goto-preview')
goto_preview.setup {}

local lspconfig = require("lspconfig")
local cmp = require("cmp_nvim_lsp")

--create nvim-cmp capabilities for lsp client
local cap = cmp.default_capabilities()

local function create_commands(bufnr)
  vim.api.nvim_buf_create_user_command(bufnr, "ListWS",
    function(_)
      vim.pretty_print(vim.lsp.buf.list_workspace_folders())
    end,
    { desc = "Print all folders in LSP workspace" }
  )
  vim.api.nvim_buf_create_user_command(bufnr, "Format",
    function(_)
      vim.lsp.buf.format { async = true }
    end,
    { desc = "Format current buffer with LSP" }
  )
  vim.api.nvim_buf_create_user_command(bufnr, "GHCiToggle",
    function(_)
      haskell_tools.repl.toggle()
    end,
    { desc = "Toggle GHCi repl for current package" }
  )
  vim.api.nvim_buf_create_user_command(bufnr, "GHCiToggleBuffer",
    function(_)
      haskell_tools.repl.toggle(vim.fn.expand("%"))
    end,
    { desc = "Toggle GHCi repl for current buffer" }
  )
  vim.api.nvim_buf_create_user_command(bufnr, "GHCiClose",
    function(_)
      haskell_tools.repl.quit()
    end,
    { desc = "Close GHCi repl window" }
  )
  vim.api.nvim_create_autocmd("BufWritePre", {
    callback = function() vim.lsp.buf.format { async = true } end
  })
end

-- callback function on lsp buffer attatch
-- define keymaps for LSP buffers
local function callback(_, bufnr)
  local opts = { noremap = true, silent = true, buffer = bufnr }
  nmap("gd", vim.lsp.buf.definition, opts) -- go to definition
  nmap("gD", vim.lsp.buf.declaration, opts) -- go to declaration
  nmap("gr", vim.lsp.buf.references, opts) -- list all refs in qf
  nmap("gi", vim.lsp.buf.implementation, opts) -- list imps in qf

  nmap("gpd", goto_preview.goto_preview_definition, opts)
  nmap("gpt", goto_preview.goto_preview_type_definition, opts)
  nmap("gpr", goto_preview.goto_preview_references, opts)
  nmap("gpc", goto_preview.close_all_win, opts)

  nmap("K", vim.lsp.buf.hover, opts)
  nmap("<leader>h", vim.lsp.buf.signature_help, opts)
  nmap("<leader>d", vim.lsp.buf.type_definition, opts)
  nmap("<leader>ca", vim.lsp.buf.code_action, opts)
  nmap("<leader>rn", ":IncRename ", opts)
  -- nmap("<leader>rn", vim.lsp.buf.rename, opts)

  nmap("<leader>wa", vim.lsp.buf.add_workspace_folder, opts)
  nmap("<leader>wr", vim.lsp.buf.remove_workspace_folder, opts)

  nmap("<space>cl", vim.lsp.codelens.run, opts)

  nmap("<leader>hh", haskell_tools.hoogle.hoogle_signature, opts)

  -- nmap("<leader>ws", cmd "ListWS", opts)
  create_commands(bufnr)
end

-- disable diagnostic inline virtual text and sign column, keep underline
-- use trouble as interface to diagnostics
vim.diagnostic.config { virtual_text = false, signs = false }

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
  sumneko_lua = sumneko,
}

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

-- setup haskell tools
haskell_tools.setup {
  hls = conf,
  tools = {
    repl = { handler = "toggleterm" }
  },
}
