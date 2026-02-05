#!/usr/bin/env bash
set -euo pipefail

max_ws="${HYPR_MAX_WORKSPACE:-9}"
delta="${1:-}"

case "${delta}" in
  +1|-1) ;;
  next) delta="+1" ;;
  prev) delta="-1" ;;
  *)
    exit 2
    ;;
esac

cur="$(hyprctl activeworkspace -j | jq -r '.id' 2>/dev/null || true)"
if ! [[ "${cur}" =~ ^[0-9]+$ ]]; then
  exit 0
fi

if (( cur < 1 )); then
  cur=1
elif (( cur > max_ws )); then
  cur="${max_ws}"
fi

if [[ "${delta}" == "+1" ]]; then
  if (( cur >= max_ws )); then
    nxt=1
  else
    nxt=$((cur + 1))
  fi
else
  if (( cur <= 1 )); then
    nxt="${max_ws}"
  else
    nxt=$((cur - 1))
  fi
fi

hyprctl dispatch workspace "${nxt}" >/dev/null 2>&1 || true

