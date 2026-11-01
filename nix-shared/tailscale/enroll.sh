#!/usr/bin/env bash
set -euo pipefail

log() {
  printf 'tailscale-enroll: %s\n' "$*" >&2
}

# A failing CLI can still return valid NeedsLogin JSON. Invalid/absent status
# never authorizes a state-changing command.
status_json="$("$TAILSCALE" status --json --peers=false 2>/dev/null || true)"
if ! state="$(printf '%s' "$status_json" | "$JQ" -er '.BackendState | select(type == "string")' 2>/dev/null)"; then
  log "daemon status unavailable; leaving login unchanged"
  exit 0
fi

# Expiry repair is explicit per profile. Bootstrap-only profiles never open a
# credential for an enrolled node, even when that node's key has expired.
reauth=false
if [[ "${REAUTHENTICATE_EXPIRED:-false}" == true && ( "$state" == Running || "$state" == NeedsLogin ) ]]; then
  key_expiry="$(printf '%s' "$status_json" | "$JQ" -r '.Self.KeyExpiry // empty' 2>/dev/null || true)"
  if [[ -n "$key_expiry" ]]; then
    expiry_epoch="$("$DATE" -d "$key_expiry" +%s 2>/dev/null || true)"
    now_epoch="$("$DATE" +%s)"
    if [[ "$expiry_epoch" =~ ^[0-9]+$ && "$now_epoch" =~ ^[0-9]+$ ]] && (( expiry_epoch <= now_epoch )); then
      reauth=true
    fi
  fi
fi

case "$state" in
  Running)
    # Check this before touching the credential: an expired or removed bootstrap
    # key must never become a reboot dependency for an enrolled node.
    if [[ "$reauth" != true ]]; then
      log "already running; keeping the existing identity and preferences"
      exit 0
    fi
    ;;
  NeedsLogin)
    # An expired existing node can also report NeedsLogin. Do not force reauth,
    # replace its account, or create another node during a remote rebuild.
    if [[ "$reauth" != true ]] && ! printf '%s' "$status_json" | "$JQ" -e '
      (((.Self // {}) | type) == "object") and
      (((.CurrentTailnet // {}) | type) == "object") and
      (((.TailscaleIPs // []) | type) == "array") and
      (((.Self.TailscaleIPs // []) | type) == "array") and
      ((.Self.ID // "") == "") and
      (((.TailscaleIPs // []) | length) == 0) and
      (((.Self.TailscaleIPs // []) | length) == 0) and
      ((.CurrentTailnet.Name // "") == "")
    ' >/dev/null; then
      log "existing enrollment or ambiguous status; interactive recovery required"
      exit 0
    fi
    ;;
  *)
    # In particular, Stopped is intentional and NeedsMachineAuth needs admin
    # approval, not a second login. Starting/unknown states can retry later.
    log "state $state; leaving login unchanged"
    exit 0
    ;;
esac

if [[ ! -f "$AUTH_KEY_FILE" || ! -r "$AUTH_KEY_FILE" || ! -s "$AUTH_KEY_FILE" ]]; then
  log "no runtime enrollment credential; skipping"
  exit 0
fi

# Follow runtime secret symlinks (for example an independently scoped decryptor)
# and verify the actual file. Never print or interpolate its content.
read -r owner mode < <("$STAT" -L -c '%u %a' -- "$AUTH_KEY_FILE")
if [[ "$owner" != 0 || ( "$mode" != 400 && "$mode" != 600 ) ]]; then
  log "credential must be root-owned with mode 0400 or 0600"
  exit 1
fi

# Preserve the optional-key convention used by existing encrypted profiles.
if [[ "$(< "$AUTH_KEY_FILE")" == DISABLED ]]; then
  log "automatic enrollment disabled by credential provisioner"
  exit 0
fi

args=(
  up
  --auth-key "file:$AUTH_KEY_FILE"
  --hostname "$NODE_HOSTNAME"
  "--accept-dns=$ACCEPT_DNS"
  --timeout=30s
)
if [[ -n "$OPERATOR" ]]; then
  args+=("--operator=$OPERATOR")
fi
if [[ -n "$ADVERTISE_TAGS" ]]; then
  args+=("--advertise-tags=$ADVERTISE_TAGS")
fi
if [[ "$reauth" == true ]]; then
  args+=(--force-reauth)
fi

# Suppress auth errors/URLs from the journal. A failure retries later without
# logout, --reset, or any daemon/network restart. Only profiles that explicitly
# opt in can force reauthentication, and only after verifying node-key expiry.
if "$TAILSCALE" "${args[@]}" >/dev/null 2>&1; then
  log "enrollment submitted; check status and any device approval"
else
  log "enrollment failed; check credential expiry and tailnet approval"
  exit 1
fi
