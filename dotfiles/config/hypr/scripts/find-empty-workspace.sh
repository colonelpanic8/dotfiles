#!/usr/bin/env bash
set -euo pipefail

# Print an "empty" workspace id within 1..$HYPR_MAX_WORKSPACE (default 9).
#
# Preference order (lowest id wins within each tier):
# 1. Workspace exists on the target monitor and has 0 windows
# 2. Workspace id does not exist at all (will be created on dispatch)
# 3. Workspace exists (elsewhere) and has 0 windows
#
# Usage:
#   find-empty-workspace.sh [monitor] [exclude_id]

max_ws="${HYPR_MAX_WORKSPACE:-9}"

monitor="${1:-}"
exclude_id="${2:-}"

if [[ -z "${monitor}" ]]; then
  monitor="$(hyprctl activeworkspace -j | jq -r '.monitor' 2>/dev/null || true)"
fi

if [[ -z "${monitor}" || "${monitor}" == "null" ]]; then
  exit 1
fi

workspaces_json="$(hyprctl workspaces -j 2>/dev/null || echo '[]')"

unused_candidate=""
elsewhere_empty_candidate=""

for i in $(seq 1 "${max_ws}"); do
  if [[ -n "${exclude_id}" && "${i}" == "${exclude_id}" ]]; then
    continue
  fi

  exists="$(jq -r --argjson id "${i}" '[.[] | select(.id == $id)] | length' <<<"${workspaces_json}")"
  if [[ "${exists}" == "0" ]]; then
    if [[ -z "${unused_candidate}" ]]; then
      unused_candidate="${i}"
    fi
    continue
  fi

  windows="$(jq -r --argjson id "${i}" '([.[] | select(.id == $id) | .windows] | .[0]) // 0' <<<"${workspaces_json}")"
  if [[ "${windows}" != "0" ]]; then
    continue
  fi

  ws_monitor="$(jq -r --argjson id "${i}" '([.[] | select(.id == $id) | .monitor] | .[0]) // ""' <<<"${workspaces_json}")"
  if [[ "${ws_monitor}" == "${monitor}" ]]; then
    printf '%s\n' "${i}"
    exit 0
  fi

  if [[ -z "${elsewhere_empty_candidate}" ]]; then
    elsewhere_empty_candidate="${i}"
  fi
done

if [[ -n "${unused_candidate}" ]]; then
  printf '%s\n' "${unused_candidate}"
  exit 0
fi

if [[ -n "${elsewhere_empty_candidate}" ]]; then
  printf '%s\n' "${elsewhere_empty_candidate}"
  exit 0
fi

exit 1

