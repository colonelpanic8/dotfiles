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

if command -v set_multiplexer_title >/dev/null 2>&1; then
  set_multiplexer_title "$title"
else
  hook_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
  "$hook_dir/../../lib/functions/set_multiplexer_title" "$title"
fi
