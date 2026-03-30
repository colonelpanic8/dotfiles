#!/usr/bin/env bash

max_ws="${HYPR_MAX_WORKSPACE:-9}"

runtime_dir="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
state_dir="${HYPR_WORKSPACE_STATE_DIR:-${runtime_dir}/hypr}"
state_file="${state_dir}/workspace-history.json"
# shellcheck disable=SC2034  # Sourced by sibling scripts that coordinate updates.
lock_file="${state_dir}/workspace-history.lock"

mkdir -p "${state_dir}"

wh_default_state() {
	cat <<'EOF'
{"monitorCurrent":{},"monitorHistory":{},"cycle":{"active":false}}
EOF
}

wh_load_state() {
	local state
	if [[ -s "${state_file}" ]] && state="$(jq -c '.' "${state_file}" 2>/dev/null)"; then
		printf '%s\n' "${state}"
		return
	fi

	wh_default_state
}

wh_save_state() {
	local state="$1"
	local tmp_file

	tmp_file="$(mktemp "${state_file}.XXXXXX")"
	printf '%s\n' "${state}" >"${tmp_file}"
	mv "${tmp_file}" "${state_file}"
}

wh_normalize_workspace() {
	local ws_id="${1:-}"
	local ws_name="${2:-}"

	if [[ -n "${ws_name}" && "${ws_name}" != "null" && "${ws_name}" != special:* ]]; then
		printf '%s\n' "${ws_name}"
		return
	fi

	if [[ -z "${ws_id}" || "${ws_id}" == "null" || "${ws_id}" =~ ^- ]]; then
		return
	fi

	if [[ "${ws_id}" =~ ^[0-9]+$ ]] && ((ws_id >= 1 && ws_id <= max_ws)); then
		printf '%s\n' "${ws_id}"
	fi
}

wh_monitors_json() {
	hyprctl -j monitors 2>/dev/null || printf '[]\n'
}

wh_refresh_state_json() {
	local state="$1"
	local monitors_json="$2"
	local row
	local monitor
	local ws_id
	local ws_name
	local workspace

	while IFS= read -r row; do
		[[ -z "${row}" ]] && continue

		monitor="$(jq -r '.name // empty' <<<"${row}")"
		ws_id="$(jq -r '.activeWorkspace.id // empty' <<<"${row}")"
		ws_name="$(jq -r '.activeWorkspace.name // empty' <<<"${row}")"
		workspace="$(wh_normalize_workspace "${ws_id}" "${ws_name}")"

		[[ -z "${monitor}" || -z "${workspace}" ]] && continue

		state="$(
			jq -c \
				--arg monitor "${monitor}" \
				--arg workspace "${workspace}" \
				'
        .monitorCurrent[$monitor] = $workspace
        | if .cycle.active == true and .cycle.monitor == $monitor then
            .
          else
            .monitorHistory[$monitor] =
              ([$workspace] + ((.monitorHistory[$monitor] // []) | map(select(. != $workspace))))
          end
        ' <<<"${state}"
		)"
	done < <(jq -c '.[]' <<<"${monitors_json}")

	printf '%s\n' "${state}"
}

wh_finalize_cycle_json() {
	local state="$1"
	local monitor
	local current

	if [[ "$(jq -r '.cycle.active // false' <<<"${state}")" != "true" ]]; then
		printf '%s\n' "${state}"
		return
	fi

	monitor="$(jq -r '.cycle.monitor // empty' <<<"${state}")"
	current="$(jq -r --arg monitor "${monitor}" '.monitorCurrent[$monitor] // empty' <<<"${state}")"

	if [[ -n "${monitor}" && -n "${current}" ]]; then
		state="$(
			jq -c \
				--arg monitor "${monitor}" \
				--arg current "${current}" \
				'
        .monitorHistory[$monitor] =
          ([$current] + ((.monitorHistory[$monitor] // []) | map(select(. != $current))))
        | .cycle = {"active": false}
        ' <<<"${state}"
		)"
	else
		state="$(jq -c '.cycle = {"active": false}' <<<"${state}")"
	fi

	printf '%s\n' "${state}"
}
