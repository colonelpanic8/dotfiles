command_exists 'open' || command_exists 'xdg-open' && alias open='xdg-open'

pasink () {
	pacmd list-sinks | grep '* index' | get_cols ' -1'
}

pavolume () {
	pactl list sinks | grep '^[[:space:]]Volume:' | \
    head -n $(( $(pasink) + 1 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,'
}
