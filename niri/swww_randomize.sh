#!/bin/bash

random_file=$(shuf -n1 -e $1/*)

# echo "$random_file"

swww img --transition-type=any --transition-step=2 --transition-duration=2 --transition-fps=120 "$random_file"
