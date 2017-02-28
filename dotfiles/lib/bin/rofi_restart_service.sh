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

function restart_service {
    result=$(get_cols 1 3)
    service_name="$(printf $result | get_cols 1 | tr -d ' ')"
    is_user="$(printf $result | get_cols 2)"
    echo "$service_name"
    case "$is_user" in
        user*)
            systemctl restart --user "$service_name"
            ;;
        system*)
            sudo systemctl restart "$service_name"
            ;;
    esac
}

{ user_units; system_units } | rofi -dmenu -i | restart_service
