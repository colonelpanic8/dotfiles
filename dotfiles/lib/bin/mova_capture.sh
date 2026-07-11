#!/usr/bin/env bash
#
# Quick-capture a task/note into mova (org-agenda-api) via rofi.
#
# Fire-and-forget: the note is written to a persistent on-disk queue and a
# background systemd user service (mova-capture-drain) POSTs it to the API,
# retrying across network outages / API downtime / reboots until it lands.
# See mova_capture_drain (the worker) and nixos/mova-capture.nix (the units).
#
# Modes:
#   mova_capture.sh                 Fast text box. Enter -> Today/NEXT
#                                   (capture-n), Alt+i -> Inbox (capture-i).
#   mova_capture.sh --pick          Pick any capture template from a rofi
#                                   list (fetched live, cached for offline),
#                                   then type the note.
#   mova_capture.sh <template-key>  Skip the picker; capture straight to that
#                                   template, e.g. `mova_capture.sh capture-z`.

set -uo pipefail

state_dir="${XDG_STATE_HOME:-$HOME/.local/state}/mova-capture"
queue_dir="$state_dir/queue"
cache_file="$state_dir/templates.json"
mkdir -p "$queue_dir"

url="${MOVA_API_URL:-https://org-agenda-api.rocket-sense.duckdns.org}"
api_user="${MOVA_API_USER:-imalison}"
pass_entry="${MOVA_API_PASS_ENTRY:-org-agenda-api-imalison}"

notify() {
  command -v notify-send >/dev/null 2>&1 && notify-send -a "mova capture" "$@" || true
}

# Read a single line of text from a rofi text box. Empty dmenu input makes rofi
# a plain input field: Enter returns the typed text; custom keybindings return
# it with exit 10/11/... The caller inspects $? via the global `input_code`.
input_code=0
read_text() {
  local prompt="$1"
  shift
  local text
  text="$(printf '' | rofi -dmenu -p "$prompt" -lines 0 "$@")"
  input_code=$?
  # Collapse newlines and trim surrounding whitespace.
  text="${text//$'\n'/ }"
  text="${text#"${text%%[![:space:]]*}"}"
  text="${text%"${text##*[![:space:]]}"}"
  printf '%s' "$text"
}

enqueue() {
  local template="$1" label="$2" title="$3"
  local id tmp final
  id="$(date +%s%N)-$$-${RANDOM}"
  tmp="$queue_dir/.$id.json.tmp"
  final="$queue_dir/$id.json"

  if ! jq -n \
      --arg template "$template" \
      --arg title "$title" \
      --arg label "$label" \
      --arg created "$(date -Is)" \
      '{template: $template, values: {Title: $title}, label: $label, created: $created}' \
      >"$tmp"; then
    notify "Capture FAILED to queue" "$title"
    rm -f "$tmp"
    return 1
  fi
  mv "$tmp" "$final"

  # Kick the drain now for immediacy; the .path unit would fire it anyway, and
  # the .timer retries anything that does not land. Fall back to a direct run
  # if systemd is unavailable (e.g. non-NixOS host).
  if ! systemctl --user start mova-capture-drain.service 2>/dev/null; then
    if command -v mova_capture_drain >/dev/null 2>&1; then
      setsid mova_capture_drain >/dev/null 2>&1 &
    fi
  fi

  notify "Queued → $label" "$title"
}

# Echo the template map ({key:{name:...}}) as JSON. Prefers a live fetch (and
# refreshes the cache), falls back to the cache, then to a minimal built-in set
# so the picker still works fully offline.
fetch_templates() {
  local pw json
  pw="$(pass show "$pass_entry" 2>/dev/null | head -1)"
  if [[ -n "$pw" ]]; then
    json="$(curl -sS -m 8 -u "$api_user:$pw" "$url/capture-templates" 2>/dev/null)"
    if [[ -n "$json" ]] && printf '%s' "$json" | jq -e 'type == "object"' >/dev/null 2>&1; then
      printf '%s' "$json" >"$cache_file"
      printf '%s' "$json"
      return 0
    fi
  fi
  if [[ -r "$cache_file" ]]; then
    cat "$cache_file"
    return 0
  fi
  printf '%s' '{"capture-n":{"name":"Next (Scheduled Today)"},"capture-i":{"name":"Inbox"},"default":{"name":"GTD Todo"}}'
}

# Resolve a template's display name from the cache without touching the network.
name_for_key() {
  local key="$1"
  [[ -r "$cache_file" ]] || return 0
  jq -r --arg k "$key" '.[$k].name // empty' "$cache_file" 2>/dev/null || true
}

mode="default"
forced_template=""
case "${1-}" in
  "") mode="default" ;;
  --pick | -p | pick) mode="pick" ;;
  -h | --help)
    sed -n '3,25p' "$0"
    exit 0
    ;;
  *)
    mode="forced"
    forced_template="$1"
    ;;
esac

case "$mode" in
  default)
    text="$(read_text "Capture" \
      -mesg '<b>Enter</b> Today · NEXT      <b>Alt+i</b> Inbox' \
      -kb-custom-1 "Alt+i")"
    [[ $input_code -eq 1 ]] && exit 0
    [[ -n "$text" ]] || exit 0
    case $input_code in
      0) enqueue "capture-n" "Today · NEXT" "$text" ;;
      10) enqueue "capture-i" "Inbox" "$text" ;;
    esac
    ;;

  pick)
    templates="$(fetch_templates)"
    selection="$(printf '%s' "$templates" \
      | jq -r 'to_entries[] | "\(.value.name)\t\(.key)"' \
      | rofi -dmenu -i -p "Template")"
    [[ -n "$selection" ]] || exit 0
    template="${selection##*$'\t'}"
    label="${selection%%$'\t'*}"
    text="$(read_text "$label")"
    [[ $input_code -eq 1 ]] && exit 0
    [[ -n "$text" ]] || exit 0
    enqueue "$template" "$label" "$text"
    ;;

  forced)
    label="$(name_for_key "$forced_template")"
    [[ -n "$label" ]] || label="$forced_template"
    text="$(read_text "$label")"
    [[ $input_code -eq 1 ]] && exit 0
    [[ -n "$text" ]] || exit 0
    enqueue "$forced_template" "$label" "$text"
    ;;
esac
