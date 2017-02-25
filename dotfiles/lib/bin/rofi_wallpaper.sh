#!/usr/bin/env zsh

find ~/Pictures/wallpaper/use -type f -or -type l | rofi -dmenu | xargs wallpaper.sh
