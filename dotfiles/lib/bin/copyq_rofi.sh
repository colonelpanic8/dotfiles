#!/usr/bin/env bash

SELECTED_INDEX="$(copyq_all.sh | rofi -dmenu -format i)"

if [ "$SELECTED_INDEX" -eq "$SELECTED_INDEX" ] 2>/dev/null; then
    copyq select "$SELECTED_INDEX"
    copyq paste
fi
