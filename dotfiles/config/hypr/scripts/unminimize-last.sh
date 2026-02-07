#!/usr/bin/env bash
# Restore a minimized window by moving it out of a special workspace.
#
# Usage: unminimize-last.sh <name>
# Example: unminimize-last.sh minimized

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

ACTIVE_JSON="$(hyprctl -j activewindow 2>/dev/null || true)"
ACTIVE_ADDR="$(printf '%s' "$ACTIVE_JSON" | jq -r '.address // empty')"
ACTIVE_WS="$(printf '%s' "$ACTIVE_JSON" | jq -r '.workspace.name // empty')"
MONITOR_ID="$(printf '%s' "$ACTIVE_JSON" | jq -r '.monitor // empty')"

# Destination is the normal active workspace for the active monitor.
DEST_WS="$(
  hyprctl -j monitors 2>/dev/null \
    | jq -r --argjson mid "${MONITOR_ID:-0}" '.[] | select(.id == $mid) | .activeWorkspace.name' \
    | head -n 1 \
    || true
)"
if [ -z "$DEST_WS" ] || [ "$DEST_WS" = "null" ]; then
  DEST_WS="$(hyprctl -j activeworkspace 2>/dev/null | jq -r '.name // empty' || true)"
fi
if [ -z "$DEST_WS" ] || [ "$DEST_WS" = "null" ]; then
  exit 0
fi

# If we're focused on a minimized window already, restore that one.
ADDR=""
if [ "$ACTIVE_WS" = "$SPECIAL_WS" ] && [ -n "$ACTIVE_ADDR" ] && [ "$ACTIVE_ADDR" != "null" ]; then
  ADDR="$ACTIVE_ADDR"
else
  # Otherwise, restore the "most recent" minimized window we can find.
  # focusHistoryID tends to have 0 as most recent; pick the smallest value.
  ADDR="$(
    hyprctl -j clients 2>/dev/null \
      | jq -r --arg sw "$SPECIAL_WS" '
          [ .[]
            | select(.workspace.name == $sw)
            | { addr: .address, fh: (.focusHistoryID // 999999999) }
          ]
          | sort_by(.fh)
          | (.[0].addr // empty)
        ' \
      | head -n 1 \
      || true
  )"
fi

if [ -z "$ADDR" ] || [ "$ADDR" = "null" ]; then
  exit 0
fi

hyprctl dispatch movetoworkspacesilent "${DEST_WS},address:${ADDR}" >/dev/null 2>&1 || true
hyprctl dispatch focuswindow "address:${ADDR}" >/dev/null 2>&1 || true

# If the minimized special workspace is currently visible, close it so we don't
# leave things in a special state after a restore.
SPECIAL_OPEN="$(
  hyprctl -j monitors 2>/dev/null \
    | jq -r --arg n "$SPECIAL_WS" --argjson mid "${MONITOR_ID:-0}" '
        .[]
        | select(.id == $mid)
        | (.specialWorkspace.name // "")
        | select(. == $n)
      ' \
    | head -n 1 \
    || true
)"
if [ -n "$SPECIAL_OPEN" ]; then
  hyprctl dispatch togglespecialworkspace "$NAME" >/dev/null 2>&1 || true
fi

exit 0

