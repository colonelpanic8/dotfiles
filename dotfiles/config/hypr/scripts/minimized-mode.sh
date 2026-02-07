#!/usr/bin/env bash
# Enter a "picker" mode for minimized windows:
# - Ensure the minimized special workspace is visible on the active monitor
# - Switch Hyprland into a submap so Enter restores and Escape cancels
#
# Usage: minimized-mode.sh <name>

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

# Ensure it's visible (but don't toggle it off if already open).
if [ "$OPEN" != "$SPECIAL_WS" ]; then
  hyprctl dispatch togglespecialworkspace "$NAME" >/dev/null 2>&1 || true
fi

hyprctl dispatch submap minimized >/dev/null 2>&1 || true
exit 0

