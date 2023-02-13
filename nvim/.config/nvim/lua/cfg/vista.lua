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

-- vista keymaps

-- \v open the vista window
nmap('<leader>v',  cmd "Vista!!")
-- \vt toggle the vista window
nmap('<leader>vt', cmd "Vista!!")
-- \vf focus open vista window
nmap('<leader>vf', cmd "Vista focus")
-- \vc: close open vista window
nmap('<leader>vc', cmd "Vista!")
-- \vm: open markdown toc
nmap('<leader>vm', cmd "Vista toc")

-- \vl open vista using nvim lsp
nmap('<leader>vl', cmd 'Vista nvim_lsp')
-- \vt open vista using ctags
nmap('<leader>vt', cmd 'Vista ctags')
