-- An example .AutoRemoteSync.json file would look like this:
--
-- {
--   "type": "rsync",
--         "remote": {
--                 "host": "example.com",
--                 "user": "root",
--                 "path": "/var/www/html"
--         },
--         "verbose": true
-- }
--
-- this plugin only works with ssh-agent public key authentication and
-- already unlocked key.
local function is_autosync_dir()
  local file = vim.fn.getcwd() .. '/' .. '.AutoRemoteSync.json'

  if vim.fn.filereadable(file) == 1 then
    vim.fn['AutoRemoteSync#Enable']()
  end
end

vim.api.nvim_create_autocmd({'DirChanged', 'VimEnter'}, {
  callback = is_autosync_dir
})
