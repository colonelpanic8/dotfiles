#!/usr/bin/env zsh

pahelper.sh | tr -d '\n' | tr '>' '|' | rofi -i -dmenu -sep '|' | grep -Eo ' [0-9]*' | xargs pahelper.sh
