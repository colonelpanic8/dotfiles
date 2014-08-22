function make_me_synergy() {
    local new_host_name="$(echo $SSH_CONNECTION | get_cols 1)"
    while getopts "h:" OPTCHAR; do
        case $OPTCHAR in
            h)
                new_host_name="$OPTARG";
                return
                ;;
        esac
    done
    test -z $(get_synergy_pids_for_ip $new_host_name) && synergyc $new_host_name
}

function stop_synergy_at() {
    pgrep synergys | xargs kill
    ssh $1 "source ~/.zshrc && clear_my_synergy"
}

function clear_all_synergy() {
    pgrep synergy | xargs kill
}

function clear_my_synergy() {
    clear_synergy_for_ip "$(echo $SSH_CONNECTION | get_cols 1)"
}

function get_synergy_pids_for_ip() {
    echo $1
    ps aux | grep synergyc | grep $1 | get_cols 2
}

function clear_synergy_for_ip() {
    get_synergy_pids_for_ip $1 | xargs kill
}

function activate_synergy_for() {
    test -z $(pgrep synergys) && synergys --config ~/synergy.conf
    ssh $1 "source ~/.zshrc && make_me_synergy"
}
