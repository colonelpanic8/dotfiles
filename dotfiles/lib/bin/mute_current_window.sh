#!/usr/bin/env zsh

otherinputs () {
	get_sink_input_info.hs | jq 'select(.application_process_id != "'"$1"'")'
}

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

if [[ $1 == *"only"* ]]; then
	pactl set-sink-input-mute "$sinkID" 0
	otherinputs "$thePID" | jq -r .sink_input_id | xargs -I theid sh -c 'pactl set-sink-input-mute theid 1'
else
	pactl set-sink-input-mute "$sinkID" "$newState"
fi
