local function diff_source()
  local gitsigns = vim.b.gitsigns_status_dict
  if gitsigns then
    return {
      added = gitsigns.added,
      modified = gitsigns.changed,
    removed = gitsigns.removed
  }
  end
end

local get_byte = [[%b 0x%B]]

require('lualine').setup {
  options = {
    icons_enabled = true,
    -- component_separators = { left = '', right = '' },
    -- section_separators = { left = '', right = '' },
    section_separators = '',
    component_separators = '|',
    disabled_filetypes = {},
    always_divide_middle = true,
    globalstatus = true,
  },
  sections = {
    -- show only the first character of current vim mode
    lualine_a = {'mode'},
    lualine_b = {
      { 'FugitiveHead', icon = '', },
      { 'diff', source = diff_source },
      { 'diagnostics', sources = { 'nvim_lsp', 'nvim_diagnostic', }, },
    },
    lualine_c = {'filename'},
    lualine_x = {get_byte, 'encoding', 'fileformat', 'filetype'},

    lualine_y = {'progress', 'filesize'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  extensions = {
    'fugitive',
    'fzf',
    'man',
    'nvim-dap-ui',
    'quickfix',
  },
}
