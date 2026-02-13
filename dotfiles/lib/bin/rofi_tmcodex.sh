#!/usr/bin/env bash
set -euo pipefail

# Pick a directory via rofi, then start Codex in a tmux session rooted there.
# Remembers previously used directories in:
#   ${XDG_STATE_HOME:-~/.local/state}/rofi-tmcodex/dirs

state_dir="${XDG_STATE_HOME:-$HOME/.local/state}/rofi-tmcodex"
history_file="$state_dir/dirs"
mkdir -p "$state_dir"
touch "$history_file"

emit_candidates() {
  # 1) Previously-used dirs (most recent first).
  cat "$history_file" 2>/dev/null || true

  # 2) A few common roots.
  for d in \
    "$HOME/dotfiles" \
    "$HOME/dotfiles/nixos" \
    "$HOME/Projects" \
    "$HOME/config" \
    "$HOME/org"
  do
    [[ -d "$d" ]] && printf '%s\n' "$d"
  done

  # 3) Shallow git repo discovery under a few likely roots.
  if command -v fd >/dev/null 2>&1; then
    local root gitdir
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

dedup() {
  awk 'NF && !seen[$0]++'
}

selected_dir="$(
  emit_candidates | dedup | rofi -dmenu -i -p 'tmcodex dir'
)"

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
  # Fail quietly; rofi launchers generally shouldn't spam terminals.
  exit 1
fi

# Update history (most recent first, unique, cap size).
tmp="$(mktemp)"
{
  printf '%s\n' "$selected_dir"
  cat "$history_file"
} | dedup | head -n 200 >"$tmp"
mv -f "$tmp" "$history_file"

# rofi launches typically have no controlling terminal. Start the agent detached
# inside tmux (tmux allocates a pty for the window). You can attach later.
base="$(basename -- "$selected_dir")"
ck="$(printf '%s' "$selected_dir" | cksum | awk '{print $1}')"
session="codex-${base}-${ck}"
# tmux is picky; keep session name simple.
session="$(printf '%s' "$session" | tr -cs 'A-Za-z0-9_-' '-' | sed 's/^-//;s/-$//')"

if tmux has-session -t "$session" 2>/dev/null; then
  exec tmux new-window -t "$session" -c "$selected_dir" codex --dangerously-bypass-approvals-and-sandbox
else
  exec tmux new-session -d -s "$session" -c "$selected_dir" codex --dangerously-bypass-approvals-and-sandbox
fi
