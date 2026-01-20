#!/usr/bin/env bash
# Replace focused window with selected window (like XMonad's myReplaceWindow)
# Swaps the positions of focused window and selected window

set -euo pipefail

# Get current workspace and focused window
CURRENT_WS=$(hyprctl activeworkspace -j | jq -r '.id')
FOCUSED=$(hyprctl activewindow -j | jq -r '.address')

if [ "$FOCUSED" = "null" ] || [ -z "$FOCUSED" ]; then
    notify-send "Replace Window" "No focused window"
    exit 0
fi

# Get all windows except focused
WINDOWS=$(hyprctl clients -j | jq -r ".[] | select(.workspace.id >= 0 and .address != \"$FOCUSED\") | \"\(.title) [\(.class)] - WS:\(.workspace.id) |\(.address)\"")

if [ -z "$WINDOWS" ]; then
    notify-send "Replace Window" "No other windows available"
    exit 0
fi

# Show rofi menu
SELECTION=$(echo "$WINDOWS" | rofi -dmenu -i -p "Replace with" -format 's')

if [ -n "$SELECTION" ]; then
    # Extract the window address
    ADDRESS=$(echo "$SELECTION" | sed 's/.*|//')

    # Swap windows using hy3
    hyprctl dispatch hy3:movewindow "address:$ADDRESS"
fi
