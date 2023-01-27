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

require("lualine").setup {
  options = {
    icons_enabled = true,
    -- section_separators = "",
    -- component_separators = "|",
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
  tabline = {},
  extensions = {
    "fugitive",
    "man",
    "nvim-dap-ui",
    "quickfix",
    "fzf",
  },
}
