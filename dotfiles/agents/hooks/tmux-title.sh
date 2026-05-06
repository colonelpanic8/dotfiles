#!/usr/bin/env bash
set -euo pipefail

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
sys.stdout.write(str(data.get("session_id") or ""))
sys.stdout.write("\0")
PY
)
cwd="${parsed[0]:-}"
prompt="${parsed[1]:-}"
session_id="${parsed[2]:-}"

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

explicit_retitle=false
case "$lower" in
  "new task:"*|"new topic:"*|"switch topic:"*|"switch context:"*|"rename title:"*|"title:"*)
    explicit_retitle=true
    task=$(printf '%s' "$prompt_first_line" | sed -E 's/^[^:]+:[[:space:]]*//')
    if [[ -z "$task" ]]; then
      task="work"
    fi
    ;;
esac

# Trim to a reasonable length for multiplexer UI labels.
if [[ ${#task} -gt 60 ]]; then
  task="${task:0:57}..."
fi

title="$project - $task"

# The hook only sees the newest prompt, not the full conversation. Avoid
# degrading a useful same-project title into a granular follow-up summary.
if [[ -n "${TMUX:-}" ]]; then
  multiplexer="tmux"
elif [[ -n "${ZELLIJ:-}" ]]; then
  multiplexer="zellij"
else
  multiplexer=""
fi

hook_state_file=""
if [[ -n "$multiplexer" ]]; then
  state_dir="${HOME}/.agents/state"
  if [[ -n "$session_id" ]]; then
    safe_session_id=$(printf '%s' "$session_id" | tr -c '[:alnum:]_.-' '_')
    hook_state_file="${state_dir}/${multiplexer}-title-hook-${safe_session_id}"
  else
    hook_state_file="${state_dir}/${multiplexer}-title"
  fi

  if [[ -f "$hook_state_file" ]]; then
    established_title=$(cat "$hook_state_file" 2>/dev/null || true)
    if [[ "$established_title" == "$project - "* && "$established_title" != "$title" && "$explicit_retitle" != true ]]; then
      exit 0
    fi
  fi
fi

if command -v set_multiplexer_title >/dev/null 2>&1; then
  set_multiplexer_title "$title"
else
  hook_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
  "$hook_dir/../../lib/functions/set_multiplexer_title" "$title"
fi

if [[ -n "$hook_state_file" ]]; then
  mkdir -p "$(dirname "$hook_state_file")"
  printf '%s' "$title" > "$hook_state_file"
fi
