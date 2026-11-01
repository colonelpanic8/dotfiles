#!/usr/bin/env bash
set -euo pipefail

if [[ "$(desktop_dmenu --backend)" == dms ]]; then
  exec dms ipc call clipboard toggle
fi

if [[ "${XDG_SESSION_TYPE:-}" == wayland ]]; then
  cliphist list | desktop_dmenu -p "Clipboard" | cliphist decode | wl-copy
else
  exec rofi -modi "clipboard:greenclip print" -show clipboard
fi
