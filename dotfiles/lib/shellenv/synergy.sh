SYNERGY_CONF="$HOME/.synergy.conf"

synergy_start_client_at() {
    ssh "$1" 'test -z $(pgrep synergyc) || synergyc '"$(localip)"'; ps aux | grep synergyc'
}

synergy_start_server_here() {
    test -z "$(pgrep synergys)" && synergys --config "$SYNERGY_CONF"
}

synergy_init_with_client() {
    synergy_start_server_here
    synergy_start_client_at "$1"
}

synergy_use_ssh_connection_as_server() {
    local new_host_name
    new_host_name="$(echo "$SSH_CONNECTION" | get_cols 1)"
    OPTIND=1
    while getopts "h:" OPTCHAR; do
        case $OPTCHAR in
            h)
                new_host_name="$OPTARG";
                return
                ;;
        esac
    done
    test -z "$(synergy_pids_for_ip "$new_host_name")" && synergyc "$new_host_name"
}

synergy_stop_at() {
    pgrep synergys | xargs kill -9
    ssh "$1" "synergy_kill_all_local"
}

synergy_kill_all_local() {
    pgrep synergy | xargs kill -9
}

synergy_clear_ssh_synergy() {
    synergy_clear_for_ip "$(echo "$SSH_CONNECTION" | get_cols 1)"
}

synergy_clear_for_ip() {
    synergy_pids_for_ip "$1" | xargs kill -9
}

synergy_pids_for_ip() {
    pgrep synergyc | grep "$1" | get_cols 2
}
