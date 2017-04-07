#!/usr/bin/env zsh

thePID="$(xprop _NET_WM_PID -id $(xdotool getactivewindow) | grep -Eo '[0-9]*')"
sinkInfo="$(pashowinputbypid $thePID)"
sinkID="$(echo $sinkInfo | jq -r .sink_input_id)"
muted="$(echo $sinkInfo | jq -r .Mute)"
if [[ $muted == *"no"* ]]; then
	newState="1"
else
	newState="0"
fi

echo "$sinkID"

pactl set-sink-input-mute "$sinkID" "$newState"
