local whitespace = require("whitespace-nvim")

whitespace.setup {
    ignored_filetypes = {
        'diff',
        'fugitive',
        'fzf',
        'help',
        'lazy',
        'lspinfo',
        'markdown',
        'quickfix',
        'TelescopePrompt',
        'toggleterm',
        'Trouble',
    }
}

vim.api.nvim_create_autocmd("BufWritePre", { callback = whitespace.trim })
