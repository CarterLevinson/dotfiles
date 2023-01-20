local fzf = require('fzf-lua')

fzf.setup { winopts = { preview = { default = 'bat' } } }

Nmap("<leader>f", fzf.files)
Nmap("<leader>ff", fzf.live_grep)
Nmap("<leader>fb", fzf.buffers)
Nmap("<leader>fg", fzf.git_files)
Nmap("<leader>fm", fzf.marks)
-- Nmap("<leader>fR", fzf.registers)

Nmap("<leader>fd", fzf.lsp_definitions)
Nmap("<leader>fD", fzf.lsp_declarations)
Nmap("<leader>fr", fzf.lsp_references)
Nmap("<leader>fs", fzf.lsp_document_symbols)
Nmap("<leader>fS", fzf.lsp_workspace_symbols)
Nmap("<leader>fi", fzf.lsp_implementations)

Imap("<C-s>", fzf.grep_cword)
