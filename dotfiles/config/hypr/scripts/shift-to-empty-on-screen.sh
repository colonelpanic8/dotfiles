#!/usr/bin/env bash
# Shift window to empty workspace on screen in given direction
# Like XMonad's shiftToEmptyOnScreen
# Usage: shift-to-empty-on-screen.sh <direction: u|d|l|r>

set -euo pipefail

DIRECTION="$1"
max_ws="${HYPR_MAX_WORKSPACE:-9}"

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

# Find an empty workspace within 1..$HYPR_MAX_WORKSPACE.
EMPTY_WS="$(~/.config/hypr/scripts/find-empty-workspace.sh "${MONITOR}" 2>/dev/null || true)"
if [[ -z "${EMPTY_WS}" ]]; then
    # No empty workspace available within the cap; restore focus and bail.
    hyprctl dispatch focusmonitor "$ORIG_MONITOR"
    exit 0
fi

if (( EMPTY_WS < 1 || EMPTY_WS > max_ws )); then
    hyprctl dispatch focusmonitor "$ORIG_MONITOR"
    exit 0
fi

# Ensure the workspace exists on the target monitor
hyprctl dispatch workspace "$EMPTY_WS"

# Go back to original monitor and move the window (without following)
hyprctl dispatch focusmonitor "$ORIG_MONITOR"
hyprctl dispatch movetoworkspacesilent "$EMPTY_WS"
