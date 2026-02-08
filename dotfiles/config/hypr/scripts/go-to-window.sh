#!/usr/bin/env bash
# Go to a window selected via rofi (with icons from desktop entries).
# Replaces "rofi -show window" which doesn't work well on Wayland.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/window-icon-map.sh"

# Get all windows on regular workspaces as TSV
WINDOW_DATA=$(hyprctl clients -j | jq -r '
    .[] | select(.workspace.id >= 0)
    | [.address, .class, (.title | gsub("\t"; " ")), (.workspace.id | tostring)]
    | @tsv')

[ -n "$WINDOW_DATA" ] || exit 0

addresses=()
TMPFILE=$(mktemp)
trap 'rm -f "$TMPFILE"' EXIT

while IFS=$'\t' read -r address class title ws_id; do
    icon=$(icon_for_class "$class")
    addresses+=("$address")
    printf '%-20s %-40s  WS:%s\0icon\x1f%s\n' \
        "$class" "${title:0:40}" "$ws_id" "$icon"
done <<< "$WINDOW_DATA" > "$TMPFILE"

INDEX=$(rofi -dmenu -i -show-icons -p "Go to window" -format i < "$TMPFILE") || exit 0

if [ -n "$INDEX" ] && [ -n "${addresses[$INDEX]:-}" ]; then
    hyprctl dispatch focuswindow "address:${addresses[$INDEX]}"
fi
