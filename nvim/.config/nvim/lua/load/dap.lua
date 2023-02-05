return {
  {
    "rcarriga/nvim-dap-ui",
    dependencies = "mfussenegger/nvim-dap",
    config = function()
      require("cfg.dap")
    end,
    enabled = false,
  }
}
