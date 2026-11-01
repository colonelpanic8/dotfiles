#!/usr/bin/env zsh

get_all_executables () {
	for exec_path in $path; do
        test -r "$exec_path" && \
		    find "$exec_path" -maxdepth 1 -executable ! -type d  -printf "%f\n"
	done
}

get_all_functions () {
	print -l ${(ok)functions}
}

get_all_aliases () {
	alias | cut -d = -f 1
}

get_all_history () {
	cat ~/.zsh_history | cut -d ';' -f 2- | awk '!a[$0]++'
}


selected=$({ get_all_executables; get_all_aliases; get_all_functions; get_all_history; } |
			   rofi -dmenu -i -kb-custom-1 "Alt+c" -kb-custom-2 "Alt+t")

rofi_exit="$?"
args=""
case "$rofi_exit" in
	1)
		exit
		;;
	10)
		args=$(echo "" | rofi -dmenu)
		;;
	11)
		urxvt --hold -e "$selected"
		;;
esac
eval "$selected $args"
