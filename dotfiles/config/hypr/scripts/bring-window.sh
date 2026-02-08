#!/usr/bin/env bash
# Bring window to current workspace (like XMonad's bringWindow)
# Uses rofi with icons to select a window, then moves it here.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/window-icon-map.sh"

CURRENT_WS=$(hyprctl activeworkspace -j | jq -r '.id')

# Get windows on OTHER workspaces as TSV
WINDOW_DATA=$(hyprctl clients -j | jq -r --argjson cws "$CURRENT_WS" '
    .[] | select(.workspace.id >= 0 and .workspace.id != $cws)
    | [.address, .class, (.title | gsub("\t"; " ")), (.workspace.id | tostring)]
    | @tsv')

if [ -z "$WINDOW_DATA" ]; then
    notify-send "Bring Window" "No windows on other workspaces"
    exit 0
fi

addresses=()
TMPFILE=$(mktemp)
trap 'rm -f "$TMPFILE"' EXIT

while IFS=$'\t' read -r address class title ws_id; do
    icon=$(icon_for_class "$class")
    addresses+=("$address")
    printf '%-20s %-40s  WS:%s\0icon\x1f%s\n' \
        "$class" "${title:0:40}" "$ws_id" "$icon"
done <<< "$WINDOW_DATA" > "$TMPFILE"

INDEX=$(rofi -dmenu -i -show-icons -p "Bring window" -format i < "$TMPFILE") || exit 0

if [ -n "$INDEX" ] && [ -n "${addresses[$INDEX]:-}" ]; then
    ADDRESS="${addresses[$INDEX]}"
    hyprctl dispatch movetoworkspace "$CURRENT_WS,address:$ADDRESS"
    hyprctl dispatch focuswindow "address:$ADDRESS"
fi
