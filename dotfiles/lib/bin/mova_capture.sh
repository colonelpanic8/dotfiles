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
#                                   then fill in its fields.
#   mova_capture.sh <template-key>  Skip the picker; capture straight to that
#                                   template, e.g. `mova_capture.sh capture-z`.
#
# Templates with several required fields (e.g. capture-v, the vocabulary card:
# Word / Definition / Original sentence) walk the fields in order, one rofi box
# each. Single-field templates keep the one-box fast path.

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
# it with exit 10/11/...
#
# Results land in the globals `input_text` / `input_code` rather than on stdout.
# Returning the text via command substitution would run this in a subshell and
# discard `input_code`, leaving every caller to read a stale 0 -- which is what
# silently routed Alt+i to the Enter branch.
input_code=0
input_text=""
read_text() {
  local prompt="$1"
  shift
  input_text="$(printf '' | rofi -dmenu -p "$prompt" -lines 0 "$@")"
  input_code=$?
  # Collapse newlines and trim surrounding whitespace.
  input_text="${input_text//$'\n'/ }"
  input_text="${input_text#"${input_text%%[![:space:]]*}"}"
  input_text="${input_text%"${input_text##*[![:space:]]}"}"
}

# Queue a capture. VALUES is a JSON object of prompt-name -> text; TITLE is
# only used for the notification.
enqueue() {
  local template="$1" label="$2" values="$3" title="$4"
  local id tmp final
  id="$(date +%s%N)-$$-${RANDOM}"
  tmp="$queue_dir/.$id.json.tmp"
  final="$queue_dir/$id.json"

  if ! jq -n \
      --arg template "$template" \
      --argjson values "$values" \
      --arg label "$label" \
      --arg created "$(date -Is)" \
      '{template: $template, values: $values, label: $label, created: $created}' \
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

# Echo the template map ({key:{name,prompts}}) as JSON. Prefers a live fetch
# (and refreshes the cache), falls back to the cache, then to a minimal
# built-in set so the picker still works fully offline.
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

# Prompt for every required field of a template and queue the result. One rofi
# box per field, in declaration order; cancelling or leaving any field empty
# aborts the whole capture rather than sending a half-filled entry.
collect_and_enqueue() {
  local templates_json="$1" template="$2" label="$3"
  local fields field prompt values title

  fields="$(printf '%s' "$templates_json" \
    | jq -r --arg k "$template" \
        '(.[$k].prompts // []) | map(select(.required)) | .[].name' 2>/dev/null)"

  # Unknown template, or one that declares no required prompts: fall back to
  # Title, which is what the API assumes for plain TODO templates.
  [[ -n "$fields" ]] || fields="Title"

  values='{}'
  while IFS= read -r field; do
    [[ -n "$field" ]] || continue
    # Only label the box with the field name when there is more than one, so
    # the common single-field case stays as terse as it was.
    if [[ "$fields" == *$'\n'* ]]; then
      prompt="$label — $field"
    else
      prompt="$label"
    fi
    read_text "$prompt"
    [[ $input_code -eq 1 ]] && return 0
    [[ -n "$input_text" ]] || return 0
    values="$(jq -c --arg n "$field" --arg v "$input_text" '. + {($n): $v}' <<<"$values")"
  done <<<"$fields"

  # The first field becomes the entry's headline, so show it in the toast.
  title="$(jq -r --arg n "$(head -1 <<<"$fields")" '.[$n] // ""' <<<"$values")"
  enqueue "$template" "$label" "$values" "$title"
}

# Resolve a template's display name from a fetched/cached template map.
name_for_key() {
  local templates_json="$1" key="$2"
  printf '%s' "$templates_json" | jq -r --arg k "$key" '.[$k].name // empty' 2>/dev/null || true
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
    read_text "Capture" \
      -mesg '<b>Enter</b> Today · NEXT      <b>Alt+i</b> Inbox' \
      -kb-custom-1 "Alt+i"
    [[ $input_code -eq 1 ]] && exit 0
    [[ -n "$input_text" ]] || exit 0
    text="$input_text"
    case $input_code in
      0) enqueue "capture-n" "Today · NEXT" "$(jq -n --arg t "$text" '{Title: $t}')" "$text" ;;
      10) enqueue "capture-i" "Inbox" "$(jq -n --arg t "$text" '{Title: $t}')" "$text" ;;
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
    collect_and_enqueue "$templates" "$template" "$label"
    ;;

  forced)
    templates="$(fetch_templates)"
    label="$(name_for_key "$templates" "$forced_template")"
    [[ -n "$label" ]] || label="$forced_template"
    collect_and_enqueue "$templates" "$forced_template" "$label"
    ;;
esac
