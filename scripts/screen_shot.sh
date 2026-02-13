imagefile=~/Pictures/Screenshots/$(date +%Y-%b-%d--%H-%M-%S).png
maim -s -u $imagefile | xclip -selection clipboard -t image/png
