#!/usr/bin/env zsh

export SYSTEMD_COLORS=0

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

function select_service_and_act {
    result=$(rofi -dmenu -i -p "systemd unit: " \
                  -kb-custom-1 "${enable}" \
                  -kb-custom-2 "${disable}" \
                  -kb-custom-3 "${stop}" \
                  -kb-custom-4 "${restart}")

    rofi_exit="$?"
    selection="$(echo $result | sed -n 's/ \+/ /gp')"

    action="restart"
    case "$rofi_exit" in
        1)
            exit
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
    esac

    service_name="$(printf $selection | awk '{ print $1 }' | tr -d ' ')"
    is_user="$(printf $selection | awk '{ print $3 }' )"

    case "$is_user" in
        user*)
            systemctl "$action" --user "$service_name"
            ;;
        system*)
            sudo systemctl "$action" "$service_name"
            ;;
    esac
}

{ user_units; system_units; } | column -tc 1 | select_service_and_act
