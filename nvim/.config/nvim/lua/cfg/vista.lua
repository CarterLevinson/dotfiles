-- vista settings

vim.g.vista_icon_indent = { '╰─➤ ' , '├─➤ ' }
vim.g.vista_sidebar_position = 'vertical topleft'
vim.g.vista_update_on_text_changed = 1
vim.g.vista_close_on_jump = 1
vim.g.close_on_fzf_select = 1
vim.g.vista_default_executive = 'ctags'
vim.g.vista_ctags_cmd = {
  haskell  = 'hasktags -x -o - -c',
  lhaskell = 'hasktags -x -c - -c',
}

vim.g.vista_executive_for = {
  awk = 'nvim_lsp',
  bash = 'nvim_lsp',
  c = 'nvim_lsp',
  cmake = 'nvim_lsp',
  cpp = 'nvim_lsp',
  lua = 'nvim_lsp',
  python = 'nvim_lsp',
  r = 'nvim_lsp',
  html = 'nvim_lsp',
  json = 'nvim_lsp',
  css = 'nvim_lsp',
  tex = 'nvim_lsp',
}

-- \v: toggle vista window
Nmap('<leader>v',  Cmd "Vista")
--\vt: toggle a vista window
Nmap('<leader>vt', Cmd "Vista!!")
-- \vv: focus open vista window
Nmap('<leader>vv', Cmd "Vista focus")
-- \vs: jump to tag nearest cursor, only works with ctags
Nmap('<leader>vs', Cmd "Vista show")
-- \vc: open vista with ctags symbols
Nmap('<leader>vc', Cmd "Vista ctags")
-- \vl: open vista with lsp symbols
Nmap('<leader>vs', Cmd "Vista nvim_lsp")
-- \vf: search tags recursively (may be slow on large projects)
Nmap('<leader>vf', Cmd "Vista finder")
-- \vc: close open vista window
Nmap('<leader>vc', Cmd "Vista!")
