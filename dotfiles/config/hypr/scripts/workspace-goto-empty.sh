#!/usr/bin/env bash
set -euo pipefail

cur_ws="$(hyprctl activeworkspace -j | jq -r '.id' 2>/dev/null || true)"
monitor="$(hyprctl activeworkspace -j | jq -r '.monitor' 2>/dev/null || true)"

ws="$(
  ~/.config/hypr/scripts/find-empty-workspace.sh "${monitor}" "${cur_ws}" 2>/dev/null || true
)"

if [[ -z "${ws}" ]]; then
  exit 0
fi

hyprctl dispatch workspace "${ws}" >/dev/null 2>&1 || true

