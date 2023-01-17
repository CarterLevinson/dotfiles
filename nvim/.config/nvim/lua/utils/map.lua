function nmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('n', lhs, rhs, options)
end

function ntmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set({'n', 't'}, lhs, rhs, options)
end

function nxmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set({'n', 'x'}, lhs, rhs, options)
end

function imap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('i', lhs, rhs, options)
end

function ismap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set({'i', 's'}, lhs, rhs, options)
end

function tmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('t', lhs, rhs, options)
end

function vmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('v', lhs, rhs, options)
end

function xmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('x', lhs, rhs, options)
end

function smap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('s', lhs, rhs, options)
end

