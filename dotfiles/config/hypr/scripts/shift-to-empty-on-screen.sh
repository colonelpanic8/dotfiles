#!/usr/bin/env bash
# Shift window to empty workspace on screen in given direction
# Like XMonad's shiftToEmptyOnScreen
# Usage: shift-to-empty-on-screen.sh <direction: u|d|l|r>

set -euo pipefail

DIRECTION="$1"

# Track the current monitor so we can return
ORIG_MONITOR=$(hyprctl activeworkspace -j | jq -r '.monitor')

# Move focus to the screen in that direction
hyprctl dispatch focusmonitor "$DIRECTION"

# Get the monitor we're now on (target monitor)
MONITOR=$(hyprctl activeworkspace -j | jq -r '.monitor')

# If there is no monitor in that direction, bail
if [ "$MONITOR" = "$ORIG_MONITOR" ]; then
    exit 0
fi

# Find an empty workspace or create one
# First check if there's an empty workspace on this monitor
EMPTY_WS=$(hyprctl workspaces -j | jq -r ".[] | select(.windows == 0 and .monitor == \"$MONITOR\") | .id" | head -1)

if [ -z "$EMPTY_WS" ]; then
    # No empty workspace, find next available workspace number
    MAX_WS=$(hyprctl workspaces -j | jq -r 'map(.id) | max')
    EMPTY_WS=$((MAX_WS + 1))
fi

# Ensure the workspace exists on the target monitor
hyprctl dispatch workspace "$EMPTY_WS"

# Go back to original monitor and move the window (without following)
hyprctl dispatch focusmonitor "$ORIG_MONITOR"
hyprctl dispatch movetoworkspacesilent "$EMPTY_WS"
