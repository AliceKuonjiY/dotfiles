return {
    "voldikss/vim-floaterm",
    config = function()
        vim.g.floaterm_width = 0.7
        vim.g.floaterm_height = 0.8
        vim.g.floaterm_borderchars = {
            '═', '║', '═', '║', '╔', '╗', '╝', '╚'
        }
    end,
}
