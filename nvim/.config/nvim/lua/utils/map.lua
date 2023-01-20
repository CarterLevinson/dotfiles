function Nmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('n', lhs, rhs, options)
end

function NTmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set({'n', 't'}, lhs, rhs, options)
end

function NXmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set({'n', 'x'}, lhs, rhs, options)
end

function Imap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('i', lhs, rhs, options)
end

function ISmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set({'i', 's'}, lhs, rhs, options)
end

function Tmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('t', lhs, rhs, options)
end

function Vmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('v', lhs, rhs, options)
end

function Xmap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('x', lhs, rhs, options)
end

function Smap(lhs, rhs, opts)
  local options = {silent = true}
  if opts then
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set('s', lhs, rhs, options)
end

function Cmd(s)
  return "<CMD>" .. s .. "<CR>"
end
