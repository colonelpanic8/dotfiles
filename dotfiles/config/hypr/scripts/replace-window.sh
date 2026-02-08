#!/usr/bin/env bash
# Replace focused window with selected window (like XMonad's myReplaceWindow)
# Swaps the positions of focused window and selected window

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/window-icon-map.sh"

FOCUSED=$(hyprctl activewindow -j | jq -r '.address')

if [ "$FOCUSED" = "null" ] || [ -z "$FOCUSED" ]; then
    notify-send "Replace Window" "No focused window"
    exit 0
fi

# Get all windows except focused as TSV
WINDOW_DATA=$(hyprctl clients -j | jq -r --arg focused "$FOCUSED" '
    .[] | select(.workspace.id >= 0 and .address != $focused)
    | [.address, .class, (.title | gsub("\t"; " ")), (.workspace.id | tostring)]
    | @tsv')

if [ -z "$WINDOW_DATA" ]; then
    notify-send "Replace Window" "No other windows available"
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

INDEX=$(rofi -dmenu -i -show-icons -p "Replace with" -format i < "$TMPFILE") || exit 0

if [ -n "$INDEX" ] && [ -n "${addresses[$INDEX]:-}" ]; then
    hyprctl dispatch hy3:movewindow "address:${addresses[$INDEX]}"
fi
