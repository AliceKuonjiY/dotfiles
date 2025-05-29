-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")

if vim.g.neovide then
    -- Put anything you want to happen only in Neovide here
    vim.o.guifont = "JetBrainsMono NF Medium" -- text below applies for VimScript
    vim.g.neovide_cursor_animation_length = 0.050
    vim.g.neovide_cursor_short_animation_length = 0.04
    vim.g.neovide_cursor_trail_size = 0.0
    vim.g.neovide_opacity = 1.0
    vim.g.neovide_normal_opacity = 0.0
    vim.g.transparency = 1.0
    vim.g.neovide_background_color = "#82aaff"
end
