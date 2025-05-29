if status is-interactive
    # Commands to run in interactive sessions can go here
end

set -x HYPRSHOT_DIR	~/Pictures/ScreenShots
set -x EDITOR		vim
set -x PATH 		~/.local/bin $PATH
set -x PATH         ~/.cargo/bin $PATH

abbr -a c clear

alias rm='gomi'
alias vimf='vim (fzf)'
alias fastfetch='fastfetch -c ~/.config/fastfetch/paleofetch.jsonc'

zoxide init fish | source
source "$HOME/.cargo/env.fish"
