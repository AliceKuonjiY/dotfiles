#!/bin/bash

# 获取当前小时 (24小时制)
HOUR=$(date +%H)

# 判断时间段
if [ $HOUR -ge 6 ] && [ $HOUR -lt 18 ]; then
    # 白天模式
    ln -sf ~/.config/waybar/theme-day.css ~/.config/waybar/style.css
else
    # 夜间模式
    ln -sf ~/.config/waybar/theme-night.css ~/.config/waybar/style.css
fi

# 重启 waybar 使更改生效
killall waybar
waybar & disown
