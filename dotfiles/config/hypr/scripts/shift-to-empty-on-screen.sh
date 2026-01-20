#!/usr/bin/env bash
# Shift window to empty workspace on screen in given direction
# Like XMonad's shiftToEmptyOnScreen
# Usage: shift-to-empty-on-screen.sh <direction: u|d|l|r>

set -euo pipefail

DIRECTION="$1"

# First, move focus to the screen in that direction
hyprctl dispatch focusmonitor "$DIRECTION"

# Get the monitor we're now on
MONITOR=$(hyprctl activeworkspace -j | jq -r '.monitor')

# Find an empty workspace or create one
# First check if there's an empty workspace on this monitor
EMPTY_WS=$(hyprctl workspaces -j | jq -r ".[] | select(.windows == 0 and .monitor == \"$MONITOR\") | .id" | head -1)

if [ -z "$EMPTY_WS" ]; then
    # No empty workspace, find next available workspace number
    MAX_WS=$(hyprctl workspaces -j | jq -r 'map(.id) | max')
    EMPTY_WS=$((MAX_WS + 1))
fi

# Go back to original monitor and move the window
hyprctl dispatch focusmonitor "$DIRECTION"  # This actually toggles back

# Move window to the empty workspace and follow
hyprctl dispatch movetoworkspace "$EMPTY_WS"
