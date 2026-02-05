#!/usr/bin/env bash
set -euo pipefail

max_ws="${HYPR_MAX_WORKSPACE:-9}"

runtime_dir="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
state_dir="${runtime_dir}/hypr"
prev_file="${state_dir}/prev-workspace"

prev="$(cat "${prev_file}" 2>/dev/null || true)"
if [[ -z "${prev}" ]]; then
  exit 0
fi

if [[ "${prev}" =~ ^[0-9]+$ ]] && (( prev < 1 || prev > max_ws )); then
  exit 0
fi

hyprctl dispatch workspace "${prev}" >/dev/null 2>&1 || true
