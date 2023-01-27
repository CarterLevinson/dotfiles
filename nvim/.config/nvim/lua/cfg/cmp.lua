local snippy = require("snippy.mapping")
local cmp_git = require("cmp_git")
-- local cmp_dap = require("cmp_dap")
local cmp = require("cmp")


imap("<Tab>", snippy.expand_or_advance("<Tab>"))
smap("<Tab>", snippy.next("<Tab>"))
ismap("S-<Tab>", snippy.previous("S-<Tab>"))
xmap("<Tab>", snippy.cut_text, { remap = true })
nmap("g<Tab>", snippy.cut_text, { remap = true })

-- cmp mappings
local function select_next(fallback)
  if cmp.visible() then
    cmp.select_next_item()
  else
    fallback()
  end
end

local function select_prev(fallback)
  if cmp.visible() then
    cmp.select_prev_item()
  else
    fallback()
  end
end

local function close_window(fallback)
  if cmp.visible() then
    cmp.close()
  else
    fallback()
  end
end

local function abort_completion(fallback)
  if cmp.visible() then
    cmp.abort()
  else
    fallback()
  end
end

local function select_entry(fallback)
  if cmp.visible() then
    cmp.confirm({ select = true })
  else
    fallback()
  end
end

local cmp_mappings = {
  ["<C-n>"] = cmp.mapping(select_next),
  ["<C-p>"] = cmp.mapping(select_prev),

  ["<C-j>"] = cmp.mapping(select_next),
  ["<C-k>"] = cmp.mapping(select_prev),

  ["<C-a>"] = cmp.mapping(abort_completion),
  ["<C-c>"] = cmp.mapping(close_window),

  ["<C-b>"] = cmp.mapping.scroll_docs(-4),
  ["<C-f>"] = cmp.mapping.scroll_docs(4),

  ["<C-space>"] = cmp.mapping(select_entry)
}

-- icon reference: https://www.nerdfonts.com/cheat-sheet
local icons = {
  Text = " ",
  Method = " ",
  Function = " ",
  Constructor = " ",
  Field = "ﰠ",
  Variable = "",
  Class = " ",
  Interface = "ﳤ",
  Module = " ",
  Property = "襁",
  Unit = "塞",
  Value = " ",
  Keyword = " ",
  Snippet = " ",
  Color = " ",
  Enum = "練",
  File = " ",
  Reference = " ",
  Folder = " ",
  EnumMember = "ﴯ",
  Constant = " ",
  Struct = "פּ ",
  Event = "",
  Operator = "",
  TypeParameter = " ",
}

-- setup cmp
cmp.setup {
  -- nvim snippy
  snippet = {
    expand = function(args) require "snippy".expand_snippet(args.body) end,
  },
  -- borders
  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
  -- comparators
  sorting = {
    comparators = {
      cmp.config.compare.offset,
      cmp.config.compare.exact,
      cmp.config.compare.score,
      require "clangd_extensions.cmp_scores",
      require "cmp-under-comparator".under,
      cmp.config.compare.kind,
      cmp.config.compare.sort_text,
      cmp.config.compare.length,
      cmp.config.compare.order,
    },
  },
  -- keymaps
  mapping = cmp.mapping.preset.insert(cmp_mappings),
  formatting = {
    format = function(entry, item)
      item.kind = string.format('%s %s', icons[item.kind], item.kind)
      item.menu = ({
        buffer = "[Buf]",
        nvim_lsp = "[LSP]",
        nvim_lsp_signature_help = "[LSP]",
        nvim_lsp_document_symbol = "[LSP]",
        treesitter = "[TS]",
        snippy = "[Snip]",
        lua_latex_symbols = "[TeX]",
        git = "[Git]",
        path = "[Path]",
        rg = "[RG]",
        calc = "[Expr]",
        dap = "[DAP]",
        commandline = "[CMD]",
      })[entry.source.name]
      return item
    end
  },
  -- set up sources
  sources = cmp.config.sources({
    { name = "nvim_lsp_signature_help" },
    { name = "nvim_lsp" },
    { name = "snippy" },
    { name = "treesitter" },
    { name = "rg" },
    { name = "path" },
    { name = "lua_latex_symbols" },
    { name = "calc" },
  })
}

-- dadbod integration
cmp.setup.filetype({ "sql", "mysql", "plsql" }, {
  sources = cmp.config.sources({
    { name = "vim-dadbod-completion" }
  }, {
    { name = "buffer" }
  })
})

-- Set configuration for specific filetype (i.e. git commit).
cmp_git.setup {}
cmp.setup.filetype("gitcommit", {
  sources = cmp.config.sources({
    { name = "git" },
    { name = "snippy" },
    { name = "buffer" },
  })
})

cmp.setup.filetype("tex", {
  sources = cmp.config.sources({
    { name = "omni" },
    { name = "snippy" },
    { name = "buffer" },
  })
})

-- use buffer and lsp document symbol source for `/`
cmp.setup.cmdline("/", {
  mapping = cmp.mapping.preset.cmdline(cmp_mappings),
  -- view = {
  --   entries = { name = "wildmenu", separator = "|" }
  -- },
  sources = cmp.config.sources({
    { name = "nvim_lsp_document_symbol" }
  }, {
    { name = "buffer" }
  })
})

-- use cmdline & path source for ':'
cmp.setup.cmdline(":", {
  mapping = cmp.mapping.preset.cmdline(cmp_mappings),
  view = {
    entries = { name = "custom", selection_order = "near_cursor" }
  },
  sources = cmp.config.sources({
    { name = "path" },
  }, {
    { name = "cmdline" }
  }),
})
