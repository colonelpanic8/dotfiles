#!/usr/bin/env bash

# Get the brightness argument (default to empty string to just show current)
ARG="${1:-}"

# Function to get current brightness percentage
get_brightness_percentage() {
    # Get list of devices
    local devices=$(brightnessctl --list 2>/dev/null | cut -d' ' -f2 | cut -d"'" -f2)
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
        # Multiple devices - output each device's brightness
        local output=""
        for device in $devices; do
            local info=$(brightnessctl -d "$device" 2>/dev/null | grep -oP '\d+%' | head -1 | tr -d '%')
            if [ -n "$info" ]; then
                if [ -z "$output" ]; then
                    output="${device}:${info}"
                else
                    output="${output} ${device}:${info}"
                fi
            fi
        done

        if [ -n "$output" ]; then
            echo "$output"
        else
            echo "50"
        fi
    fi
}

# Apply brightness change if argument provided
if [ -n "$ARG" ]; then
    # Determine if it's absolute or relative
    if [[ "$ARG" == +* ]]; then
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
    # First, try to get list of devices
    DEVICES=$(brightnessctl --list 2>/dev/null | cut -d' ' -f2 | cut -d"'" -f2)

    if [ -n "$DEVICES" ]; then
        # Apply to each device
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

# Show notification if volnoti-show is available
if command -v volnoti-show &> /dev/null; then
    volnoti-show "$BRIGHTNESS"
else
    echo "$BRIGHTNESS"
fi
