#!/usr/bin/env zsh

autorandr 2>&1 | rofi -dmenu -i | get_cols 1 | xargs autorandr -l
