local g = vim.g

-- vista settings
g.vista_icon_indent = { '╰─➤ ' , '├─➤ ' }
g.vista_sidebar_position = 'vertical topleft'
g.vista_update_on_text_changed = 1
g.vista_close_on_jump = 1
g.close_on_fzf_select = 1
g.vista_default_executive = 'ctags'
g.vista_ctags_cmd = {
  haskell  = 'hasktags -x -o - -c',
  lhaskell = 'hasktags -x -c - -c',
}

g.vista_executive_for = {
  awk = 'nvim_lsp',
  bash = 'nvim_lsp',
  c = 'nvim_lsp',
  cmake = 'nvim_lsp',
  cpp = 'nvim_lsp',
  lua = 'nvim_lsp',
  python = 'nvim_lsp',
  r = 'nvim_lsp',
  html = 'nvim_lsp',
  haskell = 'ctags',
  lhaskell = 'ctags',
  json = 'nvim_lsp',
  css = 'nvim_lsp',
  tex = 'nvim_lsp',
}

-- keymaps

-- \v: toggle vista window
nmap('<leader>v',  cmd "Vista")
--\vt: toggle a vista window
nmap('<leader>vt', cmd "Vista!!")
-- \vv: focus open vista window
nmap('<leader>vv', cmd "Vista focus")
-- \vs: jump to tag nearest cursor, only works with ctags
nmap('<leader>vs', cmd "Vista show")
-- \vc: open vista with ctags symbols
nmap('<leader>vc', cmd "Vista ctags")
-- \vl: open vista with lsp symbols
nmap('<leader>vs', cmd "Vista nvim_lsp")
-- \vf: search tags recursively (may be slow on large projects)
nmap('<leader>vf', cmd "Vista finder")
-- \vc: close open vista window
nmap('<leader>vc', cmd "Vista!")
