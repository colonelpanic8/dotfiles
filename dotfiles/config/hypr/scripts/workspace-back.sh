#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=workspace-history-common.sh
# shellcheck source-path=SCRIPTDIR
source "${script_dir}/workspace-history-common.sh"

action="${1:-cycle}"

exec 9>"${lock_file}"
flock 9

state="$(wh_load_state)"
monitors_json="$(wh_monitors_json)"
state="$(wh_refresh_state_json "${state}" "${monitors_json}")"

focused_monitor="$(
	jq -r '.[] | select(.focused == true) | .name // empty' <<<"${monitors_json}" | head -n 1
)"

if [[ -z "${focused_monitor}" ]]; then
	wh_save_state "${state}"
	exit 0
fi

current_workspace="$(jq -r --arg monitor "${focused_monitor}" '.monitorCurrent[$monitor] // empty' <<<"${state}")"

case "${action}" in
cycle)
	next_workspace="$(
		jq -r \
			--arg monitor "${focused_monitor}" \
			--arg current "${current_workspace}" \
			'
        (.monitorHistory[$monitor] // []) as $history
        | if ($history | length) < 2 then
            ""
          else
            ($history | index($current)) as $idx
            | if $idx == null then
                ""
              else
                $history[(($idx + 1) % ($history | length))]
              end
          end
        ' <<<"${state}"
	)"

	if [[ -z "${next_workspace}" || "${next_workspace}" == "${current_workspace}" ]]; then
		wh_save_state "${state}"
		exit 0
	fi

	state="$(
		jq -c \
			--arg monitor "${focused_monitor}" \
			'.cycle = {"active": true, "monitor": $monitor}' <<<"${state}"
	)"
	wh_save_state "${state}"
	hyprctl dispatch workspace "${next_workspace}" >/dev/null 2>&1 || true
	;;
finalize)
	state="$(wh_finalize_cycle_json "${state}")"
	wh_save_state "${state}"
	;;
*)
	printf 'Unknown action: %s\n' "${action}" >&2
	exit 1
	;;
esac
