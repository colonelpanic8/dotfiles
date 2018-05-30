#!/usr/bin/env zsh

ps -o 'pid cmd' x | rofi -dmenu -i | get_cols 1 | xargs kill -9
