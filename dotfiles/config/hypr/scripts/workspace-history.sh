#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=workspace-history-common.sh
# shellcheck source-path=SCRIPTDIR
source "${script_dir}/workspace-history-common.sh"

runtime_dir="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
sig="${HYPRLAND_INSTANCE_SIGNATURE:-}"
if [[ -z "$sig" ]]; then
	exit 0
fi

sock="${runtime_dir}/hypr/${sig}/.socket2.sock"

with_history_lock() {
	exec 9>"${lock_file}"
	flock 9
	"$@"
}

refresh_history_state() {
	local state
	local monitors_json

	state="$(wh_load_state)"
	monitors_json="$(wh_monitors_json)"
	state="$(wh_refresh_state_json "${state}" "${monitors_json}")"
	wh_save_state "${state}"
}

with_history_lock refresh_history_state

# Wait for the event socket to be ready.
while [[ ! -S "${sock}" ]]; do
	sleep 0.2
done

nc -U "${sock}" | while read -r line; do
	case "${line}" in
	workspace*">>"*)
		with_history_lock refresh_history_state
		;;
	esac
done
