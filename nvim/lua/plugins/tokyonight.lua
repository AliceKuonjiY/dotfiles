require("tokyonight").setup({
    style = "moon", -- 主题风格，可以是 'storm', 'moon', 'night' 或 'day'
    light_style = "day", -- default for light mode
    transparent = true, -- 启用透明背景
    terminal_colors = true, -- 配置终端中的颜色
    styles = {
        comments = { italic = true },
        keywords = { italic = true },
        functions = {},
        variables = {},
        sidebars = "transparent", -- 侧边栏样式，可以是 'dark', 'transparent' 或 'normal'
        floats = "transparent", -- 浮动窗口样式，可以是 'dark', 'transparent' 或 'normal'
    },
    sidebars = { "qf", "help" }, -- 设置侧边栏窗口的样式
    day_brightness = 0.3, -- 调整白天模式下颜色的亮度
    hide_inactive_statusline = false, -- 隐藏不活动的状态栏
    dim_inactive = false, -- 调暗不活动的窗口
    lualine_bold = true, -- 当为 true 时，lualine 主题中的部分标题将加粗显示
    on_colors = function (colors)
    end,
    on_highlights = function (highlights, colors)
        highlights.Visual = {
            bg = colors.bg_visual,
            bold = true,
        }
        highlights.CursorLine = {
        }
        highlights.LineNr = {
            fg = colors.fg_gutter,   -- 行号颜色
            bg = "none",             -- 透明背景
        }
    end,
    plugins = {},
    cache = true,
})

return {}
