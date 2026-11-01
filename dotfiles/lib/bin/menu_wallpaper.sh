#!/usr/bin/env bash

set -euo pipefail

wallpaper_dir="${WALLPAPER_DIR:-/var/lib/syncthing/sync/Wallpaper/use}"
wallpaper_variant_root="${WALLPAPER_VARIANT_ROOT:-}"

resolve_wallpaper_variant() {
  local selected="$1"
  local target_width="$2"
  local target_height="$3"
  local root="$wallpaper_variant_root"
  local selected_name selected_stem candidate

  if [ -z "$root" ]; then
    root="$(cd "$wallpaper_dir/.." && pwd -P)"
  fi

  selected_name="$(basename "$selected")"
  selected_stem="${selected_name%.*}"
  candidate="$root/${target_width}x${target_height}/$selected_stem.png"

  if [ -f "$candidate" ]; then
    printf '%s\n' "$candidate"
  else
    printf '%s\n' "$selected"
  fi
}

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
  find -L "$wallpaper_dir" -type f \
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
  local monitors monitor width height monitor_wallpaper

  if ! command -v hyprctl >/dev/null 2>&1; then
    return 1
  fi

  monitors="$(hyprctl -j monitors 2>/dev/null | jq -r \
    '.[] | [.name, .width, .height] | @tsv' 2>/dev/null || true)"
  if [ -z "$monitors" ]; then
    return 1
  fi

  while IFS=$'\t' read -r monitor width height; do
    [ -z "$monitor" ] && continue
    monitor_wallpaper="$(resolve_wallpaper_variant \
      "$selected_wallpaper" "$width" "$height")"
    hyprctl hyprpaper preload "$monitor_wallpaper" >/dev/null 2>&1 || true
    hyprctl hyprpaper wallpaper "$monitor,$monitor_wallpaper" >/dev/null 2>&1 || true
  done <<< "$monitors"

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
