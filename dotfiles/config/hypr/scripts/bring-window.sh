#!/usr/bin/env bash
# Bring window to current workspace (like XMonad's bringWindow)
# Uses rofi to select a window and moves it to the current workspace

set -euo pipefail

# Get current workspace
CURRENT_WS=$(hyprctl activeworkspace -j | jq -r '.id')

# Get all windows and format for rofi
WINDOWS=$(hyprctl clients -j | jq -r '.[] | select(.workspace.id >= 0 and .workspace.id != '"$CURRENT_WS"') | "\(.title) [\(.class)] - WS:\(.workspace.id) |\(.address)"')

if [ -z "$WINDOWS" ]; then
    notify-send "Bring Window" "No windows on other workspaces"
    exit 0
fi

# Show rofi menu
SELECTION=$(echo "$WINDOWS" | rofi -dmenu -i -p "Bring window" -format 's')

if [ -n "$SELECTION" ]; then
    # Extract the window address (after the last |)
    ADDRESS=$(echo "$SELECTION" | sed 's/.*|//')

    # Move window to current workspace
    hyprctl dispatch movetoworkspace "$CURRENT_WS,address:$ADDRESS"

    # Focus the window
    hyprctl dispatch focuswindow "address:$ADDRESS"
fi
