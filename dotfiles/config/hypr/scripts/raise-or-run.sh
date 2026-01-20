#!/usr/bin/env bash
# Raise existing window or run command (like XMonad's raiseNextMaybe)
# Usage: raise-or-run.sh <class-pattern> <command>

set -euo pipefail

CLASS_PATTERN="$1"
COMMAND="$2"

# Find windows matching the class pattern
MATCHING=$(hyprctl clients -j | jq -r ".[] | select(.class | test(\"$CLASS_PATTERN\"; \"i\")) | .address" | head -1)

if [ -n "$MATCHING" ]; then
    # Window exists, focus it
    hyprctl dispatch focuswindow "address:$MATCHING"
else
    # No matching window, run the command
    exec $COMMAND
fi
