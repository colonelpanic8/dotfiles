#!/usr/bin/env bash
set -euo pipefail

runtime_dir="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
state_dir="${runtime_dir}/hypr"
prev_file="${state_dir}/prev-workspace"

prev="$(cat "${prev_file}" 2>/dev/null || true)"
if [[ -z "${prev}" ]]; then
  exit 0
fi

hyprctl dispatch workspace "${prev}" >/dev/null 2>&1 || true
