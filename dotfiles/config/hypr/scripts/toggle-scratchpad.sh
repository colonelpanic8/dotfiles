#!/usr/bin/env bash
# Toggle a named Hyprland scratchpad, spawning it if needed.
# Usage: toggle-scratchpad.sh <name> <class_regex|-> <title_regex|-> <command...>

set -euo pipefail

if [ "$#" -lt 4 ]; then
    echo "usage: $0 <name> <class_regex|-> <title_regex|-> <command...>" >&2
    exit 1
fi

NAME="$1"
shift
CLASS_REGEX="$1"
shift
TITLE_REGEX="$1"
shift
COMMAND=("$@")

if [ "$CLASS_REGEX" = "-" ]; then
    CLASS_REGEX=""
fi
if [ "$TITLE_REGEX" = "-" ]; then
    TITLE_REGEX=""
fi

if [ -z "$CLASS_REGEX" ] && [ -z "$TITLE_REGEX" ]; then
    echo "toggle-scratchpad: provide a class or title regex" >&2
    exit 1
fi

MATCHING=$(hyprctl clients -j | jq -r --arg cre "$CLASS_REGEX" --arg tre "$TITLE_REGEX" '
  .[]
  | select(
      (($cre == "") or (.class | test($cre; "i")))
      and
      (($tre == "") or (.title | test($tre; "i")))
    )
  | .address
')

if [ -z "$MATCHING" ]; then
    "${COMMAND[@]}" &
else
    while IFS= read -r ADDR; do
        [ -n "$ADDR" ] || continue
        hyprctl dispatch movetoworkspacesilent "special:$NAME,address:$ADDR"
    done <<< "$MATCHING"
fi

hyprctl dispatch togglespecialworkspace "$NAME"
