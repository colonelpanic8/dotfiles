#!/usr/bin/env bash

# Get the brightness argument (default to empty string to just show current)
ARG="${1:-}"

# Log the incoming arguments for debugging
echo "[$(date '+%Y-%m-%d %H:%M:%S')] brightness.sh called with: '$ARG' (all args: $@)" >> /tmp/brightness_bash.log

# Function to get current brightness percentage
get_brightness_percentage() {
    # Get list of display backlight devices only (filter by class 'backlight')
    local devices=$(brightnessctl --list 2>/dev/null | grep 'class.*backlight' | cut -d' ' -f2 | cut -d"'" -f2)
    local device_count=$(echo "$devices" | wc -w)

    if [ "$device_count" -eq 0 ]; then
        echo "50"  # Default fallback
        return
    fi

    if [ "$device_count" -eq 1 ]; then
        # Single device - just output its percentage
        local info=$(brightnessctl 2>/dev/null | grep -oP '\d+%' | head -1 | tr -d '%')
        if [ -n "$info" ]; then
            echo "$info"
        else
            echo "50"
        fi
    else
        # Multiple devices - calculate average brightness
        local total=0
        local count=0
        for device in $devices; do
            local info=$(brightnessctl -d "$device" 2>/dev/null | grep -oP '\d+%' | head -1 | tr -d '%')
            if [ -n "$info" ]; then
                total=$((total + info))
                count=$((count + 1))
            fi
        done

        if [ "$count" -gt 0 ]; then
            echo $((total / count))
        else
            echo "50"
        fi
    fi
}

# Apply brightness change if argument provided
if [ -n "$ARG" ]; then
    # Determine if it's absolute or relative
    if [[ "$ARG" == "up" ]]; then
        # Increase by 5%
        BRIGHTNESS_CMD="5%+"
    elif [[ "$ARG" == "down" ]]; then
        # Decrease by 5%
        BRIGHTNESS_CMD="5%-"
    elif [[ "$ARG" == +* ]]; then
        # Relative increase (e.g., +5)
        BRIGHTNESS_CMD="${ARG:1}%+"
    elif [[ "$ARG" == -* ]]; then
        # Relative decrease (e.g., -5)
        BRIGHTNESS_CMD="${ARG:1}%-"
    else
        # Absolute value (e.g., 50)
        BRIGHTNESS_CMD="${ARG}%"
    fi

    # Try to apply to all devices
    # Get list of display backlight devices only (filter by class 'backlight')
    DEVICES=$(brightnessctl --list 2>/dev/null | grep 'class.*backlight' | cut -d' ' -f2 | cut -d"'" -f2)

    if [ -n "$DEVICES" ]; then
        # Apply to each device independently without syncing
        # This allows each device to maintain its own brightness range
        for device in $DEVICES; do
            brightnessctl -d "$device" set "$BRIGHTNESS_CMD" >/dev/null 2>&1
        done
    else
        # Fallback: just run brightnessctl without specifying device
        brightnessctl set "$BRIGHTNESS_CMD" >/dev/null 2>&1
    fi
fi

# Get current brightness percentage for display
BRIGHTNESS=$(get_brightness_percentage)
RUMNO_TIMEOUT="${RUMNO_TIMEOUT:-2.5}"

# Show notification if rumno is available
if command -v rumno &> /dev/null; then
    rumno notify -t "$RUMNO_TIMEOUT" -b "$BRIGHTNESS"
else
    echo "$BRIGHTNESS"
fi
