#!/usr/bin/env bash

set -euo pipefail

wallpaper_dir="${WALLPAPER_DIR:-/var/lib/syncthing/sync/Wallpaper/use}"

notify() {
  if command -v notify-send >/dev/null 2>&1; then
    notify-send "Wallpaper" "$1"
  else
    printf '%s\n' "$1" >&2
  fi
}

if ! command -v rofi >/dev/null 2>&1; then
  notify "rofi is not installed"
  exit 1
fi

if [ ! -d "$wallpaper_dir" ]; then
  notify "Wallpaper directory not found: $wallpaper_dir"
  exit 1
fi

mapfile -d '' wallpapers < <(
  find "$wallpaper_dir" -type f \
    \( -iname '*.png' -o -iname '*.jpg' -o -iname '*.jpeg' -o -iname '*.webp' \) \
    -print0 | sort -z
)

if [ "${#wallpapers[@]}" -eq 0 ]; then
  notify "No wallpapers found in $wallpaper_dir"
  exit 1
fi

selection=$(
  for wallpaper in "${wallpapers[@]}"; do
    printf '%s\n' "${wallpaper#"$wallpaper_dir"/}"
  done | rofi -dmenu -i -p "Wallpaper"
)

if [ -z "${selection:-}" ]; then
  exit 0
fi

selected_wallpaper="$wallpaper_dir/$selection"
if [ ! -f "$selected_wallpaper" ]; then
  notify "Wallpaper not found: $selected_wallpaper"
  exit 1
fi

apply_hyprpaper() {
  local monitor

  if ! command -v hyprctl >/dev/null 2>&1; then
    return 1
  fi

  hyprctl hyprpaper preload "$selected_wallpaper" >/dev/null 2>&1 || true

  if ! hyprctl monitors >/dev/null 2>&1; then
    return 1
  fi

  while read -r monitor; do
    [ -z "$monitor" ] && continue
    hyprctl hyprpaper wallpaper "$monitor,$selected_wallpaper" >/dev/null 2>&1 || true
  done < <(hyprctl monitors | awk '/^Monitor / { print $2 }')

  return 0
}

apply_x11() {
  if ! command -v feh >/dev/null 2>&1; then
    return 1
  fi

  feh --no-fehbg --bg-fill "$selected_wallpaper"
}

if [ -n "${HYPRLAND_INSTANCE_SIGNATURE:-}" ] || [ "${XDG_CURRENT_DESKTOP:-}" = "Hyprland" ]; then
  if apply_hyprpaper; then
    notify "Applied: ${selection}"
    exit 0
  fi
fi

if [ -n "${DISPLAY:-}" ]; then
  if apply_x11; then
    notify "Applied: ${selection}"
    exit 0
  fi
fi

notify "Unable to determine wallpaper backend for this session"
exit 1
