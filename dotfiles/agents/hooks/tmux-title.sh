#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${TMUX:-}" ]]; then
  exit 0
fi

input=$(cat)

read -r cwd prompt <<'PY' < <(python3 - <<'PY'
import json, os, sys
try:
    data = json.load(sys.stdin)
except Exception:
    data = {}

cwd = data.get("cwd") or os.getcwd()
prompt = (data.get("prompt") or "").strip()
print(cwd)
print(prompt)
PY
)

if [[ -z "${cwd}" ]]; then
  cwd="$PWD"
fi

project_root=$(git -C "$cwd" rev-parse --show-toplevel 2>/dev/null || true)
if [[ -n "$project_root" ]]; then
  project=$(basename "$project_root")
else
  project=$(basename "$cwd")
fi

prompt_first_line=$(printf '%s' "$prompt" | head -n 1 | tr '\n' ' ' | sed -e 's/[[:space:]]\+/ /g' -e 's/^ *//; s/ *$//')

lower=$(printf '%s' "$prompt_first_line" | tr '[:upper:]' '[:lower:]')
case "$lower" in
  ""|"ok"|"okay"|"thanks"|"thx"|"cool"|"yep"|"yes"|"no"|"sure"|"done"|"k")
    exit 0
    ;;
esac

task="$prompt_first_line"
if [[ -z "$task" ]]; then
  task="work"
fi

# Trim to a reasonable length for tmux status bars.
if [[ ${#task} -gt 60 ]]; then
  task="${task:0:57}..."
fi

title="$project - $task"

state_dir="${HOME}/.agents/state"
state_file="$state_dir/tmux-title"
mkdir -p "$state_dir"

if [[ -f "$state_file" ]]; then
  last_title=$(cat "$state_file" 2>/dev/null || true)
  if [[ "$last_title" == "$title" ]]; then
    exit 0
  fi
fi

printf '%s' "$title" > "$state_file"

# Update session, window, and pane titles.
tmux rename-session "$title" \; rename-window "$title" \; select-pane -T "$title"
