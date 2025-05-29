return {
    "akinsho/bufferline.nvim",
    event = "VeryLazy",
    opts = {
        options = {
            separator_style = { "", "" },
            indicator = {
                style = "icon",
                icon = "  ",
            },
            custom_areas = {
                right = function()
                    return nil
                end,
            },
        },
    },
}
