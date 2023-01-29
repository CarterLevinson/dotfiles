local fugitive_branch = {
  "FugitiveHead",
  icon = "",
}

local gitsigns_diff = {
  "diff",
  source = function()
    local gitsigns = vim.b.gitsigns_status_dict
    if gitsigns then
      return {
        added = gitsigns.added,
        modified = gitsigns.changed,
        removed = gitsigns.removed
      }
    end
  end
}

local fname = {
  "filename",
  file_status = true,
  newfile_status = true,
  symbols = {
    modified = "[+]",
    readonly = "[RO]",
    newfile = "[New]",
  }
}

local diag = {
  "diagnostics",
  sources = {
    "nvim_lsp",
    "nvim_diagnostic",
    "nvim_workspace_diagnostic",
  },
}

local display_byte = [["0x%B"]]

require("tabline").setup {
  enable = false,
  options = {
    show_filename_only = true,
    show_bufnr = true,
    show_tabs_always = true,
  }
}

require("lualine").setup {
  options = {
    icons_enabled = true,
    component_separators = { left = '', right = '' },
    section_separators = { left = '', right = '' },
    disabled_filetypes = {},
    always_divide_middle = true,
    globalstatus = true,
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { fugitive_branch, gitsigns_diff, diag },
    lualine_c = { fname },
    lualine_x = { "encoding", "fileformat", "filetype" },
    lualine_y = { "progress", "filesize", display_byte },
    lualine_z = { "location" }
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { "filename" },
    lualine_x = { "location" },
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { require "tabline".tabline_buffers },
    lualine_x = { require "tabline".tabline_tabs },
    lualine_y = {},
    lualine_z = {},
  },
  extensions = {
    "fugitive",
    "man",
    "nvim-dap-ui",
    "quickfix",
    "fzf",
  },
}
