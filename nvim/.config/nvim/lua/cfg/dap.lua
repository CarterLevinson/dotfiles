local dap = require('dap')

local get_executable_path = function()
  return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
end
-- DAP, like LSP consists of client(this plugin) and server processes

-- first configure the adapters

-- C/C++
dap.adapters.lldb = {
  type = 'executable',
  command = '/usr/bin/lldb-vscode',
  name = 'lldb',
  -- lldb-vscode by default doesn't inherit the environment variables from the
    -- parent. If you want to inherit them, add the env property definition
    -- below to your configurations entries.
    --
    -- env = function()
    --   local variables = {}
    --   for k,v in pairs(vim.fn.environ()) do
    --     table.insert(variables, string.format"%s=%s",k, v))
    --   end
    --   return variables
    -- end,
}

-- wait on setting up python dap until we understand virtual envs better
-- python
-- dap.adapters.python = {
--   type = 'executable';
--   command = 'path/to/virtualenvs/debugpy/bin/python';
--   args = { '-m', 'debugpy.adapter' };
-- }

-- Haskell
dap.adapters.haskell = {
  type = 'executable';
  command = 'haskell-debug-adapter';
  args = {'--hackage-version=0.0.33.0'};
}

-- next, configure language/filetypes

-- C/C++
dap.configurations.cpp = {
  {
    name = 'Launch',
    type = 'lldb',
    request = 'launch',
    program = get_executable_path,
    cwd = '${workspaceFolder}',
    stopOnEntry = false,
    args = {},
  },
}

dap.configurations.c = dap.configurations.cpp


-- python
-- dap.configurations.python = {
--   {
--     -- The first three options are required by nvim-dap
--     type = 'python'; -- the type here established the link to the adapter definition: `dap.adapters.python`
--     request = 'launch';
--     name = "Launch file";
--
--     -- Options below are for debugpy, see https://github.com/microsoft/debugpy/wiki/Debug-configuration-settings for supported options
--
--     program = "${file}"; -- This configuration will launch the current file if used.
--     pythonPath = function()
--       -- debugpy supports launching an application with a different interpreter then the one used to launch debugpy itself.
--       -- The code below looks for a `venv` or `.venv` folder in the current directly and uses the python within.
--       -- You could adapt this - to for example use the `VIRTUAL_ENV` environment variable.
--       local cwd = vim.fn.getcwd()
--       if vim.fn.executable(cwd .. '/venv/bin/python') == 1 then
--         return cwd .. '/venv/bin/python'
--       elseif vim.fn.executable(cwd .. '/.venv/bin/python') == 1 then
--         return cwd .. '/.venv/bin/python'
--       else
--         return '/usr/bin/python'
--       end
--     end;
--   },
-- }
--

--- Haskell
dap.configurations.haskell = {
  {
    type = 'haskell',
    request = 'launch',
    name = 'Debug',
    workspace = '${workspaceFolder}',
    startup = "${file}",
    stopOnEntry = true,
    logFile = vim.fn.stdpath('data') .. '/haskell-dap.log',
    logLevel = 'WARNING',
    ghciEnv = vim.empty_dict(),
    ghciPrompt = "λ GHCi $> ",
    -- Adjust the prompt to the prompt you see when you invoke the stack ghci command below
    ghciInitialPrompt = "λ GHCi $> ",
    ghciCmd= "stack ghci --test --no-load --no-build --main-is TARGET --ghci-options -fprint-evld-with-show",
  },
}
