is_osx && return

command_exists 'open' || command_exists 'xdg-open' && alias open='xdg-open'

pasink () {
	pacmd list-sinks | grep '\* index' | get_cols ' -1'
}

pasink() {
    pacmd stat | awk -F": " '/^Default sink name: /{print $2}'
}

pavolume () {
    pacmd list-sinks |
        awk '/^\s+name: /{indefault = $2 == "<'"$(pasink)"'>"}
            /^\s+volume: / && indefault {print $5; exit}' | grep -Eo "[0-9]*"
}

paismuted () {
	pactl list sinks | grep "$(pasink)" -A 10 | grep Mute | grep -q yes
}

pashowvolume () {
	if paismuted; then
		volnoti-show -m
	else
		volnoti-show "$(min $(pavolume) 100)"
	fi
}

pashowinputbypid () {
	get_sink_input_info.hs | jq 'select(.application_process_id == "'"$1"'")'
}
