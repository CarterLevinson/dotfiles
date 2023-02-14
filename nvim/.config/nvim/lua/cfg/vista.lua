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
-- \s open the vista window
nmap("<leader>s",  cmd "Vista!!")
-- \st toggle the vista window
nmap("<leader>st", cmd "Vista!!")
-- \sf focus open vista window
nmap("<leader>sf", cmd "Vista focus")
-- \sc: close open vista window
nmap("<leader>sc", cmd "Vista!")
-- \mm: open markdown toc
nmap("<leader>sm", cmd "Vista toc")

-- \sl open vista using nvim lsp
nmap('<leader>sl', cmd 'Vista nvim_lsp')
-- \st open vista using ctags
nmap('<leader>st', cmd 'Vista ctags')
