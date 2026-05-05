#!/usr/bin/env zsh
set -euo pipefail

# Pick a directory via rofi, then start tmcodex there.
# Candidate dirs come from previous tmcodex launches and Codex session metadata.
# tmcodex launch history is stored in:
#   ${XDG_STATE_HOME:-~/.local/state}/rofi-tmcodex/dirs

state_dir="${XDG_STATE_HOME:-$HOME/.local/state}/rofi-tmcodex"
history_file="$state_dir/dirs"
codex_home="${CODEX_HOME:-$HOME/.codex}"
terminal="${TMCODEX_TERMINAL:-${TERMINAL:-ghostty}}"
debug_log="$state_dir/debug.log"
mkdir -p "$state_dir"
touch "$history_file"

debug() {
  [[ -n "${TMCODEX_ROFI_DEBUG:-}" ]] || return 0
  printf '%s %s\n' "$(date -Is)" "$*" >>"$debug_log" 2>/dev/null || true
}

emit_candidates() {
  local root gitdir

  # 1) Explicit tmcodex history, most recent first.
  cat -- "$history_file" 2>/dev/null || true

  # 2) A few common roots. Keep these before slow/best-effort discovery so
  # rofi still has useful entries if a metadata scan breaks.
  for d in \
    "$HOME/dotfiles" \
    "$HOME/dotfiles/nixos" \
    "$HOME/Projects" \
    "$HOME/config" \
    "$HOME/org"
  do
    [[ -d "$d" ]] && printf '%s\n' "$d"
  done

  # 3) Codex session directories, newest sessions first.
  emit_codex_session_dirs

  # 4) Shallow git repo discovery under a few likely roots.
  if command -v fd >/dev/null 2>&1; then
    for root in "$HOME/Projects" "$HOME/dotfiles" "$HOME/config" "$HOME/org"; do
      [[ -d "$root" ]] || continue
      # Find ".git" directories; print their parent (repo root).
      # Keep it shallow for speed.
      while IFS= read -r gitdir; do
        printf '%s\n' "${gitdir%/.git}"
      done < <(fd -H -a -t d -d 4 '^\\.git$' "$root" 2>/dev/null || true)
    done
  fi
}

emit_codex_session_dirs() {
  local root session_file_list
  local -a session_files

  command -v jq >/dev/null 2>&1 || return 0

  session_file_list="$(
    for root in "$codex_home/sessions" "$codex_home/archived_sessions"; do
      [[ -d "$root" ]] || continue
      find "$root" -type f -name '*.jsonl' -printf '%T@ %p\n' 2>/dev/null
    done | sort -rn | awk 'NR <= 1000 { sub(/^[^ ]+ /, ""); print }'
  )"
  session_files=("${(@f)session_file_list}")

  (( ${#session_files[@]} > 0 )) || return 0
  [[ -n "${session_files[1]:-}" ]] || return 0

  awk 'FNR == 1 { print; nextfile }' "${session_files[@]}" 2>/dev/null \
    | jq -r 'select(.type == "session_meta") | .payload.cwd? // empty' 2>/dev/null \
    || true
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

debug "script=$0 codex_home=$codex_home history_file=$history_file terminal=$terminal"
selected_dir="$(
  emit_candidates | dedup | existing_dirs | rofi -dmenu -i -p 'tmcodex dir' || true
)"
debug "selected_dir=$selected_dir"

[[ -n "${selected_dir}" ]] || exit 0

# Expand ~ manually (rofi doesn't).
case "$selected_dir" in
  "~"|"~/"*)
    selected_dir="$HOME${selected_dir:1}"
    ;;
esac

if command -v realpath >/dev/null 2>&1; then
  selected_dir="$(realpath -m -- "$selected_dir" 2>/dev/null || printf '%s' "$selected_dir")"
fi

if [[ ! -d "$selected_dir" ]]; then
  if command -v notify-send >/dev/null 2>&1; then
    notify-send "tmcodex launcher" "Directory not found: $selected_dir"
  fi
  exit 1
fi

# Update history (most recent first, unique, cap size).
tmp="$(mktemp)"
{
  printf '%s\n' "$selected_dir"
  cat "$history_file"
} | awk 'NF && !seen[$0]++ && count++ < 200' >"$tmp"
mv -f "$tmp" "$history_file"

terminal_argv=(${(z)terminal})
if (( ${#terminal_argv[@]} == 0 )); then
  if command -v notify-send >/dev/null 2>&1; then
    notify-send "tmcodex launcher" "Terminal command is empty"
  fi
  exit 1
fi

("${terminal_argv[@]}" -e zsh -lc 'cd -- "$1" && exec tmcodex' zsh "$selected_dir" >/dev/null 2>&1 &!)
