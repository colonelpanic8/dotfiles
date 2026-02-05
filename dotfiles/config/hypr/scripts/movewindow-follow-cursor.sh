#!/usr/bin/env bash
# Move the active window in a direction and warp the cursor to keep its
# relative position inside the moved window.

set -euo pipefail

export PATH="/run/current-system/sw/bin:${PATH}"

if [[ $# -lt 1 ]]; then
    echo "usage: $0 <dir> [mode]" >&2
    exit 1
fi

dir="$1"
mode="${2:-}"

if ! command -v hyprctl >/dev/null; then
    exit 0
fi

move_window() {
    if [[ -n "$mode" ]]; then
        hyprctl dispatch hy3:movewindow "$dir, $mode" >/dev/null 2>&1 || true
    else
        hyprctl dispatch hy3:movewindow "$dir" >/dev/null 2>&1 || true
    fi
}

win_json="$(hyprctl -j activewindow 2>/dev/null || true)"
cur_json="$(hyprctl -j cursorpos 2>/dev/null || true)"

if [[ -z "$win_json" || "$win_json" == "null" || -z "$cur_json" || "$cur_json" == "null" ]]; then
    move_window
    exit 0
fi

win_x="$(jq -er '.at[0]' <<<"$win_json" 2>/dev/null || true)"
win_y="$(jq -er '.at[1]' <<<"$win_json" 2>/dev/null || true)"
win_w="$(jq -er '.size[0]' <<<"$win_json" 2>/dev/null || true)"
win_h="$(jq -er '.size[1]' <<<"$win_json" 2>/dev/null || true)"
cur_x="$(jq -er '.x' <<<"$cur_json" 2>/dev/null || true)"
cur_y="$(jq -er '.y' <<<"$cur_json" 2>/dev/null || true)"

if [[ ! "$win_x" =~ ^-?[0-9]+$ || ! "$win_y" =~ ^-?[0-9]+$ || ! "$win_w" =~ ^-?[0-9]+$ || ! "$win_h" =~ ^-?[0-9]+$ || ! "$cur_x" =~ ^-?[0-9]+$ || ! "$cur_y" =~ ^-?[0-9]+$ ]]; then
    move_window
    exit 0
fi

rel_x=$((cur_x - win_x))
rel_y=$((cur_y - win_y))

move_window

win_json="$(hyprctl -j activewindow 2>/dev/null || true)"
if [[ -z "$win_json" || "$win_json" == "null" ]]; then
    exit 0
fi

win_x="$(jq -er '.at[0]' <<<"$win_json" 2>/dev/null || true)"
win_y="$(jq -er '.at[1]' <<<"$win_json" 2>/dev/null || true)"
win_w="$(jq -er '.size[0]' <<<"$win_json" 2>/dev/null || true)"
win_h="$(jq -er '.size[1]' <<<"$win_json" 2>/dev/null || true)"

if [[ ! "$win_x" =~ ^-?[0-9]+$ || ! "$win_y" =~ ^-?[0-9]+$ || ! "$win_w" =~ ^-?[0-9]+$ || ! "$win_h" =~ ^-?[0-9]+$ ]]; then
    exit 0
fi

if ((rel_x < 0)); then
    rel_x=0
elif ((rel_x > win_w)); then
    rel_x=$win_w
fi

if ((rel_y < 0)); then
    rel_y=0
elif ((rel_y > win_h)); then
    rel_y=$win_h
fi

new_x=$((win_x + rel_x))
new_y=$((win_y + rel_y))

hyprctl dispatch movecursor "$new_x" "$new_y" >/dev/null 2>&1 || true
