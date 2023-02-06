local g = vim.g

-- vista settings
g.vista_icon_indent = { '╰─➤ ', '├─➤ ' }
g.vista_sidebar_position = 'vertical topleft'
g.vista_update_on_text_changed = 1
g.vista_default_executive = 'ctags'
g.vista_ctags_cmd = {
  haskell  = 'hasktags -x -o - -c',
  lhaskell = 'hasktags -x -o - -c',
}

g.vista_executive_for = {
  awk = "nvim_lsp",
  bash = "nvim_lsp",
  c = "nvim_lsp",
  cpp = "nvim_lsp",
  cmake = "nvim_lsp",
  css = "nvim_lsp",
  html = "nvim_lsp",
  json = "nvim_lsp",
  lua = "nvim_lsp",
  python = "nvim_lsp",
  r = "nvim_lsp",
}

-- vista keymap

-- \s toggle vista window
nmap('<leader>s', cmd "Vista!!")
-- \sc: close open vista window
nmap('<leader>sc', cmd "Vista!")
-- \sf: search tags recursively (may be slow on large projects)
nmap('<leader>sf', cmd "Vista finder")
-- \sl open vista using nvim lsp
nmap('<leader>sl', cmd 'Vista nvim_lsp')
-- \st open vista using ctags
nmap('<leader>st', cmd 'Vista ctags')
