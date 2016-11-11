command_exists 'open' || command_exists 'xdg-open' && alias open='xdg-open'

pavolume () {
	pactl list sinks | grep '^[[:space:]]Volume:' | \
    head -n $(( $SINK + 1 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,'
}
