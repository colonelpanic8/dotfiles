command_exists 'open' || command_exists 'xdg-open' && alias open='xdg-open'

pasink () {
	pacmd list-sinks | grep '* index' | get_cols ' -1'
}

pasink() {
    pacmd stat | awk -F": " '/^Default sink name: /{print $2}'
}

pavolume () {
    pacmd list-sinks |
        awk '/^\s+name: /{indefault = $2 == "<'$(pasink)'>"}
            /^\s+volume: / && indefault {print $5; exit}'

}
