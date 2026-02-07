#!/usr/bin/env bash
# Exit minimized picker mode:
# - Hide the minimized special workspace on the active monitor (if visible)
# - Reset the submap
#
# Usage: minimized-cancel.sh <name>

set -euo pipefail

NAME="${1:-minimized}"
NAME="${NAME#special:}"
SPECIAL_WS="special:${NAME}"

if ! command -v hyprctl >/dev/null 2>&1; then
  exit 0
fi
if ! command -v jq >/dev/null 2>&1; then
  exit 0
fi

MONITOR_ID="$(hyprctl -j activeworkspace 2>/dev/null | jq -r '.monitorID // empty' || true)"
if [ -z "$MONITOR_ID" ] || [ "$MONITOR_ID" = "null" ]; then
  MONITOR_ID=0
fi

OPEN="$(
  hyprctl -j monitors 2>/dev/null \
    | jq -r --argjson mid "$MONITOR_ID" '.[] | select(.id == $mid) | (.specialWorkspace.name // "")' \
    | head -n 1 \
    || true
)"

if [ "$OPEN" = "$SPECIAL_WS" ]; then
  hyprctl dispatch togglespecialworkspace "$NAME" >/dev/null 2>&1 || true
fi

hyprctl dispatch submap reset >/dev/null 2>&1 || true
exit 0

