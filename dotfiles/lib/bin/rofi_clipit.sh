#!/usr/bin/env bash

SELECTED_INDEX="$(clipit_history.py -r '(newline)' | rofi -dmenu -format i)"

if [ "$SELECTED_INDEX" -eq "$SELECTED_INDEX" ] 2>/dev/null; then
	xdotool type "$(clipit_history.py -e 5 -i $SELECTED_INDEX)"
fi

xdotool type
