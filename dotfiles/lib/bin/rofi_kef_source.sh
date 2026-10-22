#!/usr/bin/env bash
set -euo pipefail

notify() {
  printf '%s\n' "$1" >&2
  if command -v notify-send >/dev/null 2>&1; then
    notify-send "KEF speakers" "$1" || true
  fi
}

for dependency in rofi kef timeout; do
  if ! command -v "$dependency" >/dev/null 2>&1; then
    notify "$dependency is not installed"
    exit 1
  fi
done

choice=$(printf '%s\n' "Wi-Fi" "Bluetooth" "TV" "Optical" "Coaxial" "Analog / AUX" "Standby" |
  rofi -dmenu -i -no-custom -p "KEF source") || exit 0

case "$choice" in
  "Wi-Fi") source=wifi ;;
  "Bluetooth") source=bluetooth ;;
  "TV") source=tv ;;
  "Optical") exec kef-optical ;;
  "Coaxial") source=coaxial ;;
  "Analog / AUX") source=analog ;;
  "Standby") source=standby ;;
  "") exit 0 ;;
  *)
    notify "Unknown source: $choice"
    exit 1
    ;;
esac

model="${KEF_MODEL:-${KEF_SPEAKER_MODEL:-LS50WII}}"
discovery_timeout="${KEF_DISCOVERY_TIMEOUT:-5}"
host="${KEF_HOST:-${KEF_IP:-${KEF_SPEAKER_IP:-}}}"
kef_args=(--model "$model" --discovery-timeout "$discovery_timeout")
if [[ -n "$host" ]]; then
  kef_args=(--host "$host" "${kef_args[@]}")
fi

if [[ "$source" == standby ]]; then
  kef_args+=(standby)
else
  kef_args+=(on "$source" --timeout 30)
fi

if ! output=$(timeout --kill-after=2s 40s kef "${kef_args[@]}" 2>&1); then
  notify "Could not select $choice: $output"
  exit 1
fi

notify "Selected $choice"
