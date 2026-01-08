function fish_greeting
fastfetch
end

if status is-interactive
    # Commands to run in interactive sessions can go here
end

set -x HYPRSHOT_DIR		        ~/Pictures/ScreenShots
set -x EDITOR			        vim
set -x PATH			            ~/.local/bin $PATH
set -x PATH         		    ~/.cargo/bin $PATH
set -x PATH         		    ~/miniconda3/bin $PATH
set -x PATH         		    ~/.config/emacs/bin $PATH
set -x LLVM_CONFIG_PATH         /usr/bin/llvm-config-15
set -x SWWW_TRANSITION_STEP     10
set -x SWWW_TRANSITION          grow
set -x SWWW_TRANSITION_FPS      120
set -x SWWW_TRANSITION_POS      top-right
set -x DEEPSEEK_API_KEY         sk-8be9519672b0464cb7419e9b94a5f8b1
set -x DEEPSEEK_USE_LOCAL       false

abbr -a c clear

alias rm='gomi'
alias vimf='vim (fzf)'
alias fastfetch='fastfetch -c ~/.config/fastfetch/paleofetch.jsonc'

zoxide init fish | source
source "$HOME/.cargo/env.fish"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /home/yhu/miniconda3/bin/conda
    eval /home/yhu/miniconda3/bin/conda "shell.fish" "hook" $argv | source
else
    if test -f "/home/yhu/miniconda3/etc/fish/conf.d/conda.fish"
        . "/home/yhu/miniconda3/etc/fish/conf.d/conda.fish"
    else
        set -x PATH "/home/yhu/miniconda3/bin" $PATH
    end
end
# <<< conda initialize <<<

