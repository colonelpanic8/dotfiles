#!/usr/bin/env bash
# Minimize the active window by moving it to a special workspace without
# toggling that special workspace open.
#
# Usage: minimize-active.sh <name>
# Example: minimize-active.sh minimized

set -euo pipefail

NAME="${1:-minimized}"
NAME="${NAME#special:}"

if ! command -v hyprctl >/dev/null 2>&1; then
  exit 0
fi
if ! command -v jq >/dev/null 2>&1; then
  # We could parse plain output, but jq should exist in this setup; if it
  # doesn't, fail soft.
  exit 0
fi

ACTIVE_JSON="$(hyprctl -j activewindow 2>/dev/null || true)"
ADDR="$(printf '%s' "$ACTIVE_JSON" | jq -r '.address // empty')"
if [ -z "$ADDR" ] || [ "$ADDR" = "null" ]; then
  exit 0
fi

# If the minimized special workspace is currently visible, closing it after the
# move keeps the window hidden (what "minimize" usually means).
MONITOR_ID="$(printf '%s' "$ACTIVE_JSON" | jq -r '.monitor // empty')"
SPECIAL_OPEN="$(
  hyprctl -j monitors 2>/dev/null \
    | jq -r --arg n "special:$NAME" --argjson mid "${MONITOR_ID:-0}" '
        .[]
        | select(.id == $mid)
        | (.specialWorkspace.name // "")
        | select(. == $n)
      ' \
    | head -n 1 \
    || true
)"

hyprctl dispatch movetoworkspacesilent "special:${NAME},address:${ADDR}" >/dev/null 2>&1 || true

if [ -n "$SPECIAL_OPEN" ]; then
  hyprctl dispatch togglespecialworkspace "$NAME" >/dev/null 2>&1 || true
fi

exit 0
