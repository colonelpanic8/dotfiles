#!/usr/bin/env zsh

set -euo pipefail

otherinputs () {
	get_sink_input_info.hs | jq 'select(.application_process_id != "'"$1"'")'
}

get_active_pid () {
	if [[ "${XDG_SESSION_TYPE:-}" == "wayland" ]] && command -v hyprctl >/dev/null 2>&1; then
		hyprctl activewindow -j | jq -r '.pid'
	else
		xprop _NET_WM_PID -id "$(xdotool getactivewindow)" | grep -Eo '[0-9]*'
	fi
}

get_sink_info_by_pid () {
	local pid="$1"
	if command -v pashowinputbypid >/dev/null 2>&1; then
		pashowinputbypid "$pid"
	else
		pactl -f json list sink-inputs | jq -c --arg pid "$pid" '
			.[] | select(.properties."application.process.id" == ($pid|tonumber)) |
			{ sink_input_id: .index, Mute: (if .mute then "yes" else "no" end) }
		' | head -1
	fi
}

thePID="$(get_active_pid)"
sinkInfo="$(get_sink_info_by_pid "$thePID")"

if [[ -z "$sinkInfo" ]]; then
	exit 0
fi

sinkID="$(echo "$sinkInfo" | jq -r .sink_input_id)"
muted="$(echo "$sinkInfo" | jq -r .Mute)"
if [[ $muted == *"no"* ]]; then
	newState="1"
else
	newState="0"
fi

echo "$sinkID"

if [[ "${1:-}" == *"only"* ]]; then
	pactl set-sink-input-mute "$sinkID" 0
	otherinputs "$thePID" | jq -r .sink_input_id | xargs -I theid sh -c 'pactl set-sink-input-mute theid 1'
else
	pactl set-sink-input-mute "$sinkID" "$newState"
fi
