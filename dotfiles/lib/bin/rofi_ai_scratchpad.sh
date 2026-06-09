#!/usr/bin/env zsh
set -euo pipefail

# Choose which AI app SUPER+ALT+C toggles as a scratchpad: Codex or Claude
# Desktop. The choice is written to a state file that the Hyprland Lua config
# reads at keypress time, so switching is dynamic and needs no reload.

state_file="${XDG_STATE_HOME:-$HOME/.local/state}/hypr/ai-scratchpad"
mkdir -p "${state_file:h}"

names=(codex claude)
labels=("Codex" "Claude Desktop")

current=codex
[[ -r "$state_file" ]] && current="$(<"$state_file")"

menu=""
for i in {1..${#names}}; do
  marker="  "
  [[ "${names[$i]}" == "$current" ]] && marker="● "
  menu+="${marker}${labels[$i]}\n"
done

index="$(printf "$menu" | rofi -dmenu -i -p "AI scratchpad" -format i)" || exit 0
[[ -n "$index" ]] || exit 0

selected="${names[$((index + 1))]}"
[[ -n "$selected" ]] || exit 0

print -r -- "$selected" > "$state_file"

# Bring the freshly selected scratchpad into view (no-op if already visible).
if command -v hyprctl >/dev/null 2>&1; then
  hyprctl -q eval "_G.im_hyprland_show_ai_scratchpad()" >/dev/null 2>&1 || true
fi

if command -v notify-send >/dev/null 2>&1; then
  notify-send "AI scratchpad" "Super+Alt+C now toggles ${labels[$((index + 1))]}" || true
fi
