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

local function search_result()
  if vim.v.hlsearch == 0 then
    return ''
  end
  local last_search = vim.fn.getreg('/')
  if not last_search or last_search == '' then
    return ''
  end
  local searchcount = vim.fn.searchcount { maxcount = 9999 }
  return last_search .. '(' .. searchcount.current ..
    '/' .. searchcount.total .. ')'
end


require('lualine').setup {
  options = {
    icons_enabled = true,
    -- theme = 'molokai',
    theme = 'powerline_dark',
    component_separators = { left = '', right = '' },
    section_separators = { left = '', right = '' },
    disabled_filetypes = {},
    always_divide_middle = true,
    globalstatus = false,
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
    lualine_x = { search_result, 'encoding', 'fileformat', 'filetype'},
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
  tabline = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { require'tabline'.tabline_buffers },
    lualine_x = { require'tabline'.tabline_tabs },
    lualine_y = {},
    lualine_z = {},
  },
  extensions = {
    'fugitive',
    'fzf',
    'man',
    'nvim-dap-ui',
    'quickfix',
  },
}
