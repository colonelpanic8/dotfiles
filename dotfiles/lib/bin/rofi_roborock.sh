#!/usr/bin/env bash

set -euo pipefail

notify() {
  local message="$1"

  if command -v notify-send >/dev/null 2>&1; then
    notify-send "Roborock" "$message"
  else
    printf '%s\n' "$message" >&2
  fi
}

select_rofi() {
  local prompt="$1"
  shift

  printf '%s\n' "$@" | rofi -dmenu -i -p "$prompt"
}

require_command() {
  local command_name="$1"

  if ! command -v "$command_name" >/dev/null 2>&1; then
    notify "$command_name is not installed"
    exit 1
  fi
}

run_roborock() {
  if ! output=$(roborock-control "$@" 2>&1); then
    notify "Command failed: roborock-control $*

$output"
    exit 1
  fi
}

require_command rofi
require_command roborock-control

room_choice=$(
  select_rofi "Roborock room" \
    "Kitchen / Dining" \
    "Living Room" \
    "Entryway / Hallway" \
    "Office / Closet" \
    "Bathroom" \
    "Bedroom" \
    "Bedroom Closet"
) || exit 0

case "$room_choice" in
  "Kitchen / Dining") room="kitchen" ;;
  "Living Room") room="living-room" ;;
  "Entryway / Hallway") room="entryway-hallway" ;;
  "Office / Closet") room="office-closet" ;;
  "Bathroom") room="bathroom" ;;
  "Bedroom") room="bedroom" ;;
  "Bedroom Closet") room="bedroom-closet" ;;
  "") exit 0 ;;
  *)
    notify "Unknown room: $room_choice"
    exit 1
    ;;
esac

cleaning_choice=$(
  select_rofi "Cleaning type" \
    "Vacuum + mop" \
    "Vacuum only" \
    "Mop only"
) || exit 0

case "$cleaning_choice" in
  "Vacuum + mop") cleaning_type="vacuum_mop" ;;
  "Vacuum only") cleaning_type="vacuum" ;;
  "Mop only") cleaning_type="mop" ;;
  "") exit 0 ;;
  *)
    notify "Unknown cleaning type: $cleaning_choice"
    exit 1
    ;;
esac

suction_choice=$(
  select_rofi "Suction level" \
    "Balanced" \
    "Quiet" \
    "Turbo" \
    "Max"
) || exit 0

case "$suction_choice" in
  "Quiet") fan_power=101 ;;
  "Balanced") fan_power=102 ;;
  "Turbo") fan_power=104 ;;
  "Max") fan_power=108 ;;
  "") exit 0 ;;
  *)
    notify "Unknown suction level: $suction_choice"
    exit 1
    ;;
esac

case "$cleaning_type" in
  vacuum)
    run_roborock raw set_water_box_custom_mode "[200]"
    run_roborock segment --fan-power "$fan_power" "$room"
    ;;
  vacuum_mop)
    run_roborock raw set_custom_mode "[$fan_power]"
    run_roborock raw set_water_box_custom_mode "[203]"
    run_roborock segment --skip-max-fan "$room"
    run_roborock raw set_water_box_custom_mode "[203]"
    ;;
  mop)
    run_roborock raw set_custom_mode "[$fan_power]"
    run_roborock raw set_water_box_custom_mode "[203]"
    run_roborock segment --skip-max-fan "$room"
    run_roborock raw set_water_box_custom_mode "[203]"
    ;;
esac

notify "Started ${cleaning_choice,,} in $room_choice with $suction_choice suction"
