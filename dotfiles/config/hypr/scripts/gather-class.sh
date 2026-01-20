#!/usr/bin/env bash
# Gather all windows of the same class as focused window (like XMonad's gatherThisClass)

set -euo pipefail

# Get focused window class
FOCUSED_CLASS=$(hyprctl activewindow -j | jq -r '.class')
CURRENT_WS=$(hyprctl activeworkspace -j | jq -r '.id')

if [ "$FOCUSED_CLASS" = "null" ] || [ -z "$FOCUSED_CLASS" ]; then
    notify-send "Gather Class" "No focused window"
    exit 0
fi

# Find all windows with same class on other workspaces
WINDOWS=$(hyprctl clients -j | jq -r ".[] | select(.class == \"$FOCUSED_CLASS\" and .workspace.id != $CURRENT_WS and .workspace.id >= 0) | .address")

if [ -z "$WINDOWS" ]; then
    notify-send "Gather Class" "No other windows of class '$FOCUSED_CLASS'"
    exit 0
fi

# Move each window to current workspace
COUNT=0
for ADDR in $WINDOWS; do
    hyprctl dispatch movetoworkspace "$CURRENT_WS,address:$ADDR"
    ((COUNT++))
done

notify-send "Gather Class" "Gathered $COUNT windows of class '$FOCUSED_CLASS'"
