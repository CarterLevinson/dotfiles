local cmp = require('cmp')
local cmp_git = require('cmp_git')
local snippy = require('snippy')
local snippy_mappings = require('snippy.mapping')
local lspkind = require('lspkind')

-- snippy mappings
vim.keymap.set('i', '<Tab>', snippy_mappings.expand_or_advance('<Tab>'))
vim.keymap.set('s', '<Tab>', snippy_mappings.next('<Tab>'))
vim.keymap.set({ 'i', 's' }, '<S-Tab>', snippy_mappings.previous('<S-Tab>'))
vim.keymap.set('x', '<Tab>', snippy_mappings.cut_text, { remap = true })
vim.keymap.set('n', 'g<Tab>', snippy_mappings.cut_text, { remap = true })

-- cmp mappings
local function ctrld(fallback)
  if cmp.visible() then
    cmp.close()
  else
    fallback()
  end
end

local function ctrlj(fallback)
  if cmp.visible() then
    cmp.select_next_item()
  else
    fallback()
  end
end

local function ctrlk(fallback)
  if cmp.visible() then
    cmp.select_prev_item()
  else
    fallback()
  end
end

-- local function cr(fallback)
--   if cmp.visible() then
--     cmp.confirm({ select = true})
--   else
--     fallback()
--   end
-- end

local function cspace(fallback)
  if cmp.visible() then
    cmp.confirm({select = true})
  else
    cmp.complete()
  end
end

local cmp_mappings = {
  ['<C-n>']     = cmp.mapping.select_next_item(),
  ['<C-p>']     = cmp.mapping.select_prev_item(),

  ['<C-k>']     = cmp.mapping(ctrlk),
  ['<C-j>']     = cmp.mapping(ctrlj),

  ['<C-a>']     = cmp.mapping.abort(),
  ['<C-d>']     = cmp.mapping(ctrld),

  ['<C-b>']     = cmp.mapping.scroll_docs(-4),
  ['<C-f>']     = cmp.mapping.scroll_docs(4),

  ['<C-space>'] = cmp.mapping(cspace),
  ['<CR>']      = cmp.mapping.confirm({ select = true }),
}

-- setup cmp
cmp.setup{
  snippet = {
    expand = function(args) snippy.expand_snippet(args.body) end,
  },
  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
  mapping = cmp.mapping.preset.insert(cmp_mappings),
  formatting = {
    format = lspkind.cmp_format({
      mode = 'symbol',
      menu = {
        buffer = '[Buf]',
        nvim_lsp = '[LSP]',
        nvim_lsp_signature_help = '[LSP]',
        nvim_lsp_document_symbol = '[LSP]',
        treesitter = '[TS]',
        snippy = '[Snip]',
        latex_symbols = '[TeX]',
        nvim_lua = '[API]',
        doxygen = '[Doc]',
        git = '[Git]',
        path = '[Path]',
        rg = '[Ripgrep]',
        calc = '[Calc]',
        dap = '[DAP]',
        commandline = '[CMD]',
      },
      -- icon reference: https://www.nerdfonts.com/cheat-sheet
      symbol_map = {
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
        Operator =  "",
        TypeParameter = " ",
      },
    }),
  },
  sources = cmp.config.sources({
    { name = 'nvim_lua' }, -- smart enough to only activate only lua files
    { name = 'nvim_lsp' },
    { name = 'nvim_lsp_signature_help' },
    { name = 'buffer' },
    { name = 'snippy' },
    { name = 'treesitter' },
    { name = 'lua-latex-symbols' },
    { name = 'calc' },
    -- { name = 'rg',  max_item_count = 10 },
    { name = 'path', keyword_length = 5 },
  })
}

-- figure out how to only activiate doxygen completion in comments

-- dadbod integration
cmp.setup.filetype({'sql', 'mysql', 'plsql'}, {
  sources = cmp.config.sources({
    { name = 'vim-dadbod-completion' }
  }, {
    { name = 'buffer' }
  })
})


-- Set configuration for specific filetype (i.e. git commit).
cmp_git.setup{}
cmp.setup.filetype('gitcommit', {
  sources = cmp.config.sources({
    { name = 'git' }
  }, {
    { name = 'buffer' }
  })
})

-- set up DAP filetype completion
cmp.setup.filetype({'dap-repl', 'dapui-watches'}, {
  sources = {
    name = 'dap'
  },
})

-- use buffer and lsp document symbol source for `/`
cmp.setup.cmdline('/', {
  mapping = cmp.mapping.preset.cmdline(cmp_mappings),
  sources = cmp.config.sources({
    { name = 'nvim_lsp_document_symbol' }
  }, {
    { name = 'buffer' }
  })
})

-- use cmdline & path source for ':'
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(cmp_mappings),
  sources = cmp.config.sources({
    { name = 'path' },
  }, {
    { name = 'cmdline' }
  })
})
