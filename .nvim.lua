local ctx = require("exrc").init()

local function disable_copilot()
	vim.b.copilot_enabled = false
end

disable_copilot()
local augroup = vim.api.nvim_create_augroup("disable-copilot", { clear = true })
vim.api.nvim_create_autocmd("BufEnter", {
	group = augroup,
	callback = disable_copilot,
})


ctx:on_unload(function()
	vim.api.nvim_clear_autocmds({ group = augroup })
end)
