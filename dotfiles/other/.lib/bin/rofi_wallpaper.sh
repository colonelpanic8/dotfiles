#!/usr/bin/env zsh

find ~/Pictures/wallpaper/use -type f -or -type l | rofi -i -dmenu | xargs wallpaper.sh
