local clangd_extensions = require "clangd_extensions"
local haskell_tools = require "haskell-tools"
local rust_tools = require "rust-tools"

local lspconfig = require "lspconfig"
local cmp = require "cmp_nvim_lsp"

local neodev = require "neodev"
neodev.setup {}

local goto_preview = require "goto-preview"
goto_preview.setup {}

--create nvim-cmp capabilities for lsp client
local capabilities = cmp.default_capabilities()

local function create_lsp_user_commands(bufnr)
  vim.api.nvim_buf_create_user_command(bufnr, "ListWS",
      function(_) vim.pretty_print(vim.lsp.buf.list_workspace_folders()) end,
      { desc = "Print all folders in LSP workspace" }
  )

  vim.api.nvim_buf_create_user_command(bufnr, "Format",
      function(_) vim.lsp.buf.format { async = true } end,
      { desc = "Format current buffer with LSP" }
  )
end

local function create_lsp_keymaps(opts)
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
  nmap("<leader>D", vim.lsp.buf.type_definition, opts)
  nmap("<leader>rn", vim.lsp.buf.rename, opts)
  nmap("<leader>ca", cmd "CodeActionMenu", opts)

  nmap("<leader>wa", vim.lsp.buf.add_workspace_folder, opts)
  nmap("<leader>wr", vim.lsp.buf.remove_workspace_folder, opts)

  nmap("<leader>cl", vim.lsp.codelens.run, opts)
end

local function create_lsp_autocmds()
  vim.api.nvim_create_augroup("Format", { clear = true })

  vim.api.nvim_create_autocmd("BufWritePost", {
      group = "Format",
      pattern = { "*.cc", "*.cpp", "*.h", "*.hpp", "*.hs", "*.lhs" },
      callback = function() vim.lsp.buf.format { async = true } end,
  })
end

-- callback function on lsp buffer attatch
-- define keymaps for LSP buffers
local function callback(_, bufnr)
  create_lsp_keymaps({ noremap = true, silent = true, buffer = bufnr })
  create_lsp_user_commands(bufnr)
  create_lsp_autocmds()
end

-- disable diagnostic inline virtual text and sign column, keep underline
-- use trouble as interface to diagnostics
vim.diagnostic.config { virtual_text = false, signs = false }

local default_conf = {
    on_attach = callback,
    settings = {
        capabilities = capabilities,
        telemetry = { enable = false },
    },
    single_file_support = true,
}

local lua_conf = {
    settings = {
        Lua = {
            completion = { callSnippet = "Replace" },
            diagnostics = { globals = { "vim" } },
        }
    }
}

local haskell_conf = {
    settings = {
        haskell = {
            hlintOn = true,
            checkProject = true,
            formattingProvider = "formolu",
        }
    }
}

local clangd_conf = {
    cmd = {
        "clangd",
        "--clang-tidy",
        "--background-index",
        "--all-scopes-completion",
        "--completion-style=detailed",
        "--function-arg-placeholders",
        -- "--header-insertion=iwyu",
    },
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
    lua_ls = lua_conf,
}

-- setup lsp servers
for lsp, opts in pairs(servers) do
  local options = default_conf
  if not (next(opts) == nil) then
    options = vim.tbl_deep_extend("force", options, opts)
  end
  lspconfig[lsp].setup(options)
end

-- set up clangd lsp extensions
clangd_extensions.setup {
    server = vim.tbl_deep_extend("force", default_conf, clangd_conf)
}

-- setup rust lsp extensions
rust_tools.setup {
    server = default_conf
}

-- setup haskell tools
haskell_tools.setup {
    hls = vim.tbl_deep_extend("force", default_conf, haskell_conf),
    tools = { repl = { handler = "toggleterm" } },
}
