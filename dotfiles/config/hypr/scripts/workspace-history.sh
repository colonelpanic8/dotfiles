#!/usr/bin/env bash
set -euo pipefail

runtime_dir="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
sig="${HYPRLAND_INSTANCE_SIGNATURE:-}"
if [[ -z "$sig" ]]; then
  exit 0
fi

sock="${runtime_dir}/hypr/${sig}/.socket2.sock"
state_dir="${runtime_dir}/hypr"
last_file="${state_dir}/last-workspace"
prev_file="${state_dir}/prev-workspace"

mkdir -p "${state_dir}"

# Initialize current workspace to avoid empty state.
if command -v hyprctl >/dev/null 2>&1; then
  cur_id="$(hyprctl activeworkspace -j | jq -r '.id' 2>/dev/null || true)"
  if [[ -n "${cur_id}" && "${cur_id}" != "null" ]]; then
    echo "${cur_id}" > "${last_file}"
  fi
fi

# Wait for the event socket to be ready.
while [[ ! -S "${sock}" ]]; do
  sleep 0.2
done

nc -U "${sock}" | while read -r line; do
  case "${line}" in
    workspace*">>"*)
      payload="${line#*>>}"
      # Handle workspacev2 payloads: id,name
      if [[ "${payload}" == *","* ]]; then
        ws_id="${payload%%,*}"
        ws_name="${payload#*,}"
      else
        ws_id="${payload}"
        ws_name="${payload}"
      fi

      # Ignore special/negative workspaces.
      if [[ "${ws_id}" =~ ^- ]] || [[ "${ws_name}" == special:* ]]; then
        continue
      fi

      ws_ident="${ws_name}"
      if [[ -z "${ws_ident}" ]]; then
        ws_ident="${ws_id}"
      fi

      prev="$(cat "${last_file}" 2>/dev/null || true)"
      if [[ -n "${prev}" && "${ws_ident}" != "${prev}" ]]; then
        echo "${prev}" > "${prev_file}"
      fi
      echo "${ws_ident}" > "${last_file}"
      ;;
  esac
done
