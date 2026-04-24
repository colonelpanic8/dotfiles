#!/usr/bin/env bash
set -euo pipefail

if [[ -n "${ZELLIJ:-}" ]]; then
  multiplexer="zellij"
elif [[ -n "${TMUX:-}" ]]; then
  multiplexer="tmux"
else
  exit 0
fi

input=$(cat)

mapfile -d '' -t parsed < <(PAYLOAD="$input" python3 - <<'PY'
import json, os, sys
try:
    data = json.loads(os.environ.get("PAYLOAD", ""))
except Exception:
    data = {}

cwd = data.get("cwd") or os.getcwd()
prompt = (data.get("prompt") or "").strip()
sys.stdout.write(cwd)
sys.stdout.write("\0")
sys.stdout.write(prompt)
sys.stdout.write("\0")
PY
)
cwd="${parsed[0]:-}"
prompt="${parsed[1]:-}"

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

# Trim to a reasonable length for multiplexer UI labels.
if [[ ${#task} -gt 60 ]]; then
  task="${task:0:57}..."
fi

title="$project - $task"

state_dir="${HOME}/.agents/state"
state_file="$state_dir/${multiplexer}-title"
mkdir -p "$state_dir"

if [[ -f "$state_file" ]]; then
  last_title=$(cat "$state_file" 2>/dev/null || true)
  if [[ "$last_title" == "$title" ]]; then
    exit 0
  fi
fi

printf '%s' "$title" > "$state_file"

# Update session, window/tab, and pane titles.
if [[ "$multiplexer" == "tmux" ]]; then
  tmux rename-session "$title" \; rename-window "$title" \; select-pane -T "$title"
else
  zellij action rename-session "$title"
  zellij action rename-tab "$title"
  zellij action rename-pane "$title"
fi
