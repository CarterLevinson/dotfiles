local g = vim.g

-- vista settings
g.vista_icon_indent = { '╰─➤ ', '├─➤ ' }
g.vista_sidebar_position = 'vertical topleft'
g.vista_update_on_text_changed = 1
g.vista_close_on_jump = 1
g.close_on_fzf_select = 1
g.vista_default_executive = 'nvim_lsp'
g.vista_ctags_cmd = {
  haskell  = 'hasktags -x -o - -c',
  lhaskell = 'hasktags -x -o - -c',
}

g.vista_executive_for = {
  haskell = 'ctags',
  lhaskell = 'ctags',
}

-- vista keymap

-- \s toggle vista window
nmap('<leader>s', cmd "Vista!!")
-- \sc: close open vista window
nmap('<leader>sc', cmd "Vista!")
-- \sf: search tags recursively (may be slow on large projects)
nmap('<leader>sf', cmd "Vista finder")
