function _G.nmap(lhs, rhs, opts)
  local options = { silent = true }
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('n', lhs, rhs, options)
end

function _G.ntmap(lhs, rhs, opts)
  local options = { silent = true }
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set({ 'n', 't' }, lhs, rhs, options)
end

function _G.nxmap(lhs, rhs, opts)
  local options = { silent = true }
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set({ 'n', 'x' }, lhs, rhs, options)
end

function _G.nvomap(lhs, rhs, opts)
  local options = { silent = true }
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set({ 'n', 'v', 'o' }, lhs, rhs, options)
end

function _G.imap(lhs, rhs, opts)
  local options = { silent = true }
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('i', lhs, rhs, options)
end

function _G.ismap(lhs, rhs, opts)
  local options = { silent = true }
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set({ 'i', 's' }, lhs, rhs, options)
end

function _G.tmap(lhs, rhs, opts)
  local options = { silent = true }
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('t', lhs, rhs, options)
end

function _G.vmap(lhs, rhs, opts)
  local options = { silent = true }
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('v', lhs, rhs, options)
end

function _G.xmap(lhs, rhs, opts)
  local options = { silent = true }
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('x', lhs, rhs, options)
end

function _G.smap(lhs, rhs, opts)
  local options = { silent = true }
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('s', lhs, rhs, options)
end

function _G.cmd(s)
  return "<CMD>" .. s .. "<CR>"
end
