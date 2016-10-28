#!/usr/bin/env zsh

find ~/Pictures/wallpaper/use -type l | rofi -dmenu | xargs wallpaper.sh
