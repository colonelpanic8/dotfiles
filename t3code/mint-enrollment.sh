#!/usr/bin/env bash
# Mints a reusable enrollment on every fleet host and writes one bundle, sealed
# to the phone and agenix recipients, at nixos/secrets/t3code-fleet-enrollment.age.
# Each host exports its entry to a throwaway age key, so credentials are only
# ever plaintext in this process's memory.
#
# usage: mint-enrollment.sh [--ttl 365d] [host...]   (default: every fleet host)
set -euo pipefail

if ! command -v age >/dev/null || ! command -v jq >/dev/null; then
  exec nix shell nixpkgs#age nixpkgs#jq --command "$0" "$@"
fi

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
output="$root/nixos/secrets/t3code-fleet-enrollment.age"

ttl=365d
if [ "${1:-}" = --ttl ]; then
  ttl=$2
  shift 2
fi

manifest=$(nix eval --json -f "$root/nix-shared/t3code-fleet.nix")
if [ $# -gt 0 ]; then
  hosts=("$@")
else
  mapfile -t hosts < <(jq -r '.[].label' <<<"$manifest")
fi
recipients=$(nix eval --json -f "$root/nixos/secrets/secrets.nix" '"t3code-fleet-enrollment.age".publicKeys' | jq -r '.[]')

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
age-keygen -o "$work/identity" 2>/dev/null
ephemeral=$(age-keygen -y "$work/identity")
label="fleet bundle $(date -u +%Y-%m-%d)"

# Runs on each host as the server's user. Reads the recipient and manifest from
# stdin so nothing secret lands in argv.
remote=$(
  cat <<'EOF'
set -eu
nix=$(command -v nix || echo /nix/var/nix/profiles/default/bin/nix)
t3=$(command -v t3 || echo /etc/profiles/per-user/$USER/bin/t3)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
IFS= read -r recipient
printf '%s\n' "$recipient" >"$tmp/recipients"
cat >"$tmp/manifest.json"
T3CODE_ENVIRONMENT_ID="fleet:$HOST" "$t3" auth enrollment export \
  --recipients-file "$tmp/recipients" \
  --fleet-manifest "$tmp/manifest.json" \
  --output "$tmp/enrollment.age" \
  --label "$LABEL" --ttl "$TTL" \
  --age-bin "$("$nix" build --no-link --print-out-paths nixpkgs#age)/bin/age" \
  --json >&2
cat "$tmp/enrollment.age"
EOF
)

entries=()
for host in "${hosts[@]}"; do
  jq -e --arg h "$host" 'any(.label == $h)' <<<"$manifest" >/dev/null ||
    { echo "$host is not in nix-shared/t3code-fleet.nix" >&2; exit 1; }
  echo "minting on $host" >&2
  { printf '%s\n' "$ephemeral"; jq -c '{version: 1, environments: .}' <<<"$manifest"; } |
    ssh -o BatchMode=yes "imalison@$host" \
      "HOST=$host LABEL=$(printf %q "$label") TTL=$(printf %q "$ttl") bash -c $(printf %q "$remote")" \
      >"$work/$host.age"
  # Older servers still export a single-anchor v1 document.
  entries+=("$(age -d -i "$work/identity" "$work/$host.age" | jq -c '
    if .version == 1 then
      [{environmentId: .anchor.environmentId, label: .anchor.label,
        httpBaseUrl: .anchor.httpBaseUrl, wsBaseUrl: .anchor.wsBaseUrl,
        credential, expiresAt}]
    else .environments end')")
done

printf '%s\n' "${entries[@]}" |
  jq -cs '{version: 2, type: "t3code.fleet-enrollment", environments: add}' |
  age -e -a -R <(printf '%s\n' "$recipients") -o "$work/bundle.age"
mv "$work/bundle.age" "$output"
echo "wrote $output for: ${hosts[*]}" >&2
echo "revoke a host's grant with: t3 auth pairing revoke <id> (ids above)" >&2
