# CrossPaste only discovers peers over mDNS, which Tailscale does not carry.
# Fetch each online tailnet peer's announcement over HTTP and republish it
# through avahi so the local app finds it, then pair with any peer this user
# can reach over SSH by reading its pairing PIN there.

interval="${CROSSPASTE_TAILNET_INTERVAL:-60}"
pair_retry_seconds="${CROSSPASTE_TAILNET_PAIR_RETRY:-900}"
endpoint_file="$HOME/.local/share/.crosspaste/cli-endpoint.json"

declare -A publisher_pids=()
declare -A published_records=()
declare -A pair_attempts=()

cli_api() {
    local path="$1" socket
    shift
    socket="$(jq -r .socketPath "$endpoint_file" 2>/dev/null)" || return 1
    curl -sf --unix-socket "$socket" -H 'Content-Type: application/json' "http://localhost/cli$path" "$@"
}

stop_publisher() {
    local id="$1"
    kill "${publisher_pids[$id]}" 2>/dev/null || true
    unset "publisher_pids[$id]" "published_records[$id]"
}

publish() {
    local id="$1" sync_info="$2" port encoded
    local -a txt=()
    encoded="$(printf %s "$sync_info" | base64 -w0)"
    if [[ "${published_records[$id]:-}" == "$encoded" ]] && kill -0 "${publisher_pids[$id]:-0}" 2>/dev/null; then
        return
    fi
    [[ -n "${publisher_pids[$id]:-}" ]] && stop_publisher "$id"

    # Same chunking as CrossPaste's TxtRecordUtils.
    for ((i = 0; i * 128 < ${#encoded}; i++)); do
        txt+=("$i=${encoded:i*128:128}")
    done
    port="$(jq -r .endpointInfo.port <<<"$sync_info")"
    avahi-publish -s "crosspaste-tailnet-$id" _crosspasteService._tcp "$port" "${txt[@]}" >/dev/null 2>&1 &
    publisher_pids[$id]=$!
    published_records[$id]="$encoded"
}

pair_with() {
    local host="$1" id="$2" own_id="$3" pin_file ssh_pid session code result
    pin_file="$(mktemp)"
    # `token --wait` opens the peer's acceptance window and prints the PIN
    # once our request arrives.
    ssh -o BatchMode=yes -o ConnectTimeout=5 "$host" \
        crosspaste --no-start --json token --wait --timeout 30 >"$pin_file" 2>/dev/null &
    ssh_pid=$!
    sleep 3

    session="$(cli_api /pair/initiate -X POST -d "$(jq -cn --arg id "$id" '{appInstanceId: $id}')" | jq -r .sessionId)"
    wait "$ssh_pid" || true
    code="$(jq -r --arg id "$own_id" '.requests[]? | select(.appInstanceId == $id) | .token' "$pin_file" 2>/dev/null | head -1)"
    rm -f "$pin_file"

    if [[ -z "$session" || "$session" == null ]]; then
        echo "Could not start pairing with $host" >&2
        return
    fi
    if [[ -z "$code" ]]; then
        echo "No pairing code from $host" >&2
        cli_api /pair/cancel -X POST -d "$(jq -cn --arg s "$session" '{sessionId: $s}')" >/dev/null || true
        return
    fi
    result="$(cli_api /pair/submit -X POST -d "$(jq -cn --arg s "$session" --arg c "$code" '{sessionId: $s, code: $c}')")"
    echo "Pairing with $host: $(jq -r .message <<<"$result" 2>/dev/null)" >&2
}

sync_once() {
    local own_id paired ip host os sync_info id
    local -a candidates=()
    local -A seen=()
    own_id="$(cli_api /status | jq -r .appInstanceId)" || return
    paired="$(cli_api /devices | jq -r '.[].appInstanceId')" || return

    while IFS=$'\t' read -r ip host os; do
        sync_info="$(curl -sf -m 3 "http://$ip:13129/sync/syncInfo")" || continue
        id="$(jq -r '.appInfo.appInstanceId // empty' <<<"$sync_info")" || continue
        [[ -z "$id" || "$id" == "$own_id" ]] && continue
        seen[$id]=1
        publish "$id" "$sync_info"
        # Only the side with the smaller id initiates, so peers never race.
        if [[ "$os" == linux && "$own_id" < "$id" ]] && ! grep -qxF "$id" <<<"$paired"; then
            candidates+=("$host $id")
        fi
    done < <(tailscale status --json |
        jq -r '.Peer[]? | select(.Online) | [.TailscaleIPs[0], .HostName, .OS] | @tsv')

    for id in "${!publisher_pids[@]}"; do
        [[ -n "${seen[$id]:-}" ]] || stop_publisher "$id"
    done

    local now candidate
    now="$(date +%s)"
    for candidate in "${candidates[@]}"; do
        read -r host id <<<"$candidate"
        ((now - ${pair_attempts[$id]:-0} >= pair_retry_seconds)) || continue
        pair_attempts[$id]="$now"
        # Give the app a moment to resolve a freshly published record.
        sleep 5
        pair_with "$host" "$id" "$own_id"
    done
}

trap 'kill $(jobs -p) 2>/dev/null' EXIT

while true; do
    sync_once || true
    sleep "$interval"
done
