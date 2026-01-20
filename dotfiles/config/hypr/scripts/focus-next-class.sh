#!/usr/bin/env bash
# Focus next window of a different class (like XMonad's focusNextClass)

set -euo pipefail

# Get focused window class
FOCUSED_CLASS=$(hyprctl activewindow -j | jq -r '.class')
FOCUSED_ADDR=$(hyprctl activewindow -j | jq -r '.address')

if [ "$FOCUSED_CLASS" = "null" ] || [ -z "$FOCUSED_CLASS" ]; then
    # No focused window, just focus any window
    hyprctl dispatch cyclenext
    exit 0
fi

# Get all unique classes
ALL_CLASSES=$(hyprctl clients -j | jq -r '[.[] | select(.workspace.id >= 0) | .class] | unique | .[]')

# Get sorted list of classes
CLASSES_ARRAY=()
while IFS= read -r class; do
    CLASSES_ARRAY+=("$class")
done <<< "$ALL_CLASSES"

# Find current class index and get next class
CURRENT_INDEX=-1
for i in "${!CLASSES_ARRAY[@]}"; do
    if [ "${CLASSES_ARRAY[$i]}" = "$FOCUSED_CLASS" ]; then
        CURRENT_INDEX=$i
        break
    fi
done

if [ $CURRENT_INDEX -eq -1 ] || [ ${#CLASSES_ARRAY[@]} -le 1 ]; then
    # Only one class or class not found
    exit 0
fi

# Get next class (wrapping around)
NEXT_INDEX=$(( (CURRENT_INDEX + 1) % ${#CLASSES_ARRAY[@]} ))
NEXT_CLASS="${CLASSES_ARRAY[$NEXT_INDEX]}"

# Find first window of next class
NEXT_WINDOW=$(hyprctl clients -j | jq -r ".[] | select(.class == \"$NEXT_CLASS\" and .workspace.id >= 0) | .address" | head -1)

if [ -n "$NEXT_WINDOW" ]; then
    hyprctl dispatch focuswindow "address:$NEXT_WINDOW"
fi
