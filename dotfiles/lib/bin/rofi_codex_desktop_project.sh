#!/usr/bin/env zsh
set -euo pipefail

# Pick a saved Codex Desktop project root via rofi, then start a new desktop
# thread in that project using the documented codex:// deep link.

codex_home="${CODEX_HOME:-$HOME/.codex}"
state_file="${CODEX_DESKTOP_STATE_FILE:-$codex_home/.codex-global-state.json}"
prompt="${CODEX_DESKTOP_PROJECT_PROMPT:-Codex project}"

notify() {
  if command -v notify-send >/dev/null 2>&1; then
    notify-send "Codex Desktop launcher" "$1"
  else
    printf '%s\n' "$1" >&2
  fi
}

emit_candidates() {
  if [[ ! -r "$state_file" ]]; then
    notify "Cannot read Codex Desktop state: $state_file"
    return 1
  fi

  jq -r '
    def local_paths($key):
      .[$key] // []
      | .[]?
      | select(type == "string" and startswith("/"));

    local_paths("pinned-project-ids"),
    local_paths("electron-saved-workspace-roots"),
    local_paths("project-order")
  ' "$state_file"
}

dedup() {
  awk 'NF && !seen[$0]++'
}

existing_dirs() {
  local dir
  while IFS= read -r dir; do
    [[ -d "$dir" ]] && printf '%s\n' "$dir"
  done
}

if [[ "${1:-}" == "--print-candidates" ]]; then
  emit_candidates | dedup | existing_dirs
  exit 0
fi

selected_dir="$(
  emit_candidates | dedup | existing_dirs | rofi -dmenu -i -p "$prompt" || true
)"

[[ -n "$selected_dir" ]] || exit 0

case "$selected_dir" in
  "~"|"~/"*)
    selected_dir="$HOME${selected_dir:1}"
    ;;
esac

if command -v realpath >/dev/null 2>&1; then
  selected_dir="$(realpath -m -- "$selected_dir" 2>/dev/null || printf '%s' "$selected_dir")"
fi

if [[ ! -d "$selected_dir" ]]; then
  notify "Directory not found: $selected_dir"
  exit 1
fi

encoded_path="$(jq -rn --arg path "$selected_dir" '$path | @uri')"

if ! command -v xdg-open >/dev/null 2>&1; then
  notify "xdg-open is not available"
  exit 1
fi

xdg-open "codex://threads/new?path=$encoded_path" >/dev/null 2>&1 &!
