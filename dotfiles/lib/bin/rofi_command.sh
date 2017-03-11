#!/usr/bin/env bash
function get_all_commands {
    IFS=:
	for exec_path in $PATH; do
        test -r "$exec_path" && \
		    find "$exec_path" -maxdepth 1 -executable ! -type d  -printf "%f\n"
	done
}

selected=$(get_all_commands | rofi -dmenu -i)
"$selected"
