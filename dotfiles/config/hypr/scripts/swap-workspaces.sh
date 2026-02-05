#!/usr/bin/env bash
# Swap the contents of the current workspace with another workspace.
# Intended to mirror XMonad's swapWithCurrent behavior.

set -euo pipefail

max_ws="${HYPR_MAX_WORKSPACE:-9}"

CURRENT_WS="$(hyprctl activeworkspace -j | jq -r '.id')"
if [[ -z "${CURRENT_WS}" || "${CURRENT_WS}" == "null" ]]; then
  exit 0
fi

TARGET_WS="${1:-}"

if [[ -z "${TARGET_WS}" ]]; then
  WS_LIST="$({
    seq 1 "${max_ws}"
    hyprctl workspaces -j | jq -r '.[].id' 2>/dev/null || true
  } | awk 'NF {print $1}' | awk '!seen[$0]++' | sort -n)"

  TARGET_WS="$(printf "%s\n" "${WS_LIST}" | rofi -dmenu -p "Swap with workspace")"
fi

if [[ -z "${TARGET_WS}" || "${TARGET_WS}" == "null" ]]; then
  exit 0
fi

if [[ "${TARGET_WS}" == "${CURRENT_WS}" ]]; then
  exit 0
fi

if ! [[ "${TARGET_WS}" =~ ^-?[0-9]+$ ]]; then
  notify-send "Swap Workspace" "Invalid workspace: ${TARGET_WS}"
  exit 1
fi

if (( TARGET_WS < 1 || TARGET_WS > max_ws )); then
  notify-send "Swap Workspace" "Workspace out of range (1-${max_ws}): ${TARGET_WS}"
  exit 1
fi

WINDOWS_CURRENT="$(hyprctl clients -j | jq -r --arg ws "${CURRENT_WS}" '.[] | select((.workspace.id|tostring) == $ws) | .address')"
WINDOWS_TARGET="$(hyprctl clients -j | jq -r --arg ws "${TARGET_WS}" '.[] | select((.workspace.id|tostring) == $ws) | .address')"

for ADDR in ${WINDOWS_CURRENT}; do
  hyprctl dispatch movetoworkspace "${TARGET_WS},address:${ADDR}"
done

for ADDR in ${WINDOWS_TARGET}; do
  hyprctl dispatch movetoworkspace "${CURRENT_WS},address:${ADDR}"
done
