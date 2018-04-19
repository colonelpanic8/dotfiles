#!/usr/bin/env sh

export SYSTEMD_COLORS=0
term=${ROFI_SYSTEMD_TERM-termite -e}
default_action=${ROFI_SYSTEMD_DEFAULT_ACTION-"list_actions"}

function user_units {
	SYSTEMD_COLORS=0 systemctl --user list-unit-files | tail -n +2 | head -n -2 |
		awk '{print $0 "  user"}'
}

function system_units {
	systemctl list-unit-files | tail -n +2 | head -n -2 |
		awk '{print $0 "  system"}'
}

enable="Alt+e"
disable="Alt+d"
stop="Alt+k"
restart="Alt+r"
tail="Alt+t"
list_actions="Alt+l"

all_actions="enable
disable
stop
restart
tail
list_actions"

function select_service_and_act {
	result=$(rofi -dmenu -i -p "systemd unit: " \
	              -kb-custom-1 "${enable}" \
	              -kb-custom-2 "${disable}" \
	              -kb-custom-3 "${stop}" \
	              -kb-custom-4 "${restart}" \
	              -kb-custom-5 "${tail}" \
	              -kb-custom-6 "${list_actions}")

	rofi_exit="$?"

	case "$rofi_exit" in
		1)
			action="exit"
			exit 1
			;;
		10)
			action="enable"
			;;
		11)
			action="disable"
			;;
		12)
			action="stop"
			;;
		13)
			action="restart"
			;;
		14)
			action="tail"
			;;
		15)
			action="list_actions"
			;;
		*)
			action="$default_action"
			;;
	esac

	selection="$(echo $result | sed -n 's/ \+/ /gp')"
	service_name="$(echo $selection | awk '{ print $1 }' | tr -d ' ')"
	is_user="$(echo $selection | awk '{ print $3 }' )"

	case "$is_user" in
		user*)
			user_arg="--user"
			command="systemctl $user_arg"
			;;
		system*)
			user_arg=""
			command="sudo systemctl"
			;;
		*)
			command="systemctl"
	esac

	to_run="$(get_command_with_args)"
	echo "Running $to_run"
	eval "$to_run"
}

function get_command_with_args {
	case "$action" in
		"tail")
			echo "$term 'journalctl $user_arg -u $service_name -f'"
			;;
		"list_actions")
			action=$(echo "$all_actions" | rofi -dmenu -i -p "Select action: ")
			get_command_with_args
			;;
		*)
			echo "$command $action $service_name"
			;;
	esac
}

{ user_units; system_units; } | column -tc 1 | select_service_and_act
