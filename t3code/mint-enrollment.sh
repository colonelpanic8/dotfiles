#!/usr/bin/env bash
# Mints a reusable enrollment on every host of a fleet and writes one bundle,
# sealed to the phone and agenix recipients, at
# nixos/secrets/t3code-fleet-enrollment[.<fleet>].age. Each host exports its
# entry to a throwaway age key, so credentials are only ever plaintext in this
# process's memory.
#
# usage: mint-enrollment.sh [--fleet ivan|alex] [--ttl 36500d] [host...]
#        (default: Ivan's fleet, every host in it)
set -euo pipefail

if ! command -v age >/dev/null || ! command -v jq >/dev/null; then
  exec nix shell nixpkgs#age nixpkgs#jq --command "$0" "$@"
fi

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
fleet=ivan
# The server requires a finite TTL; a century means the enrollment never
# lapses, and `t3 auth pairing revoke` still withdraws it.
ttl=36500d
while [ $# -gt 0 ]; do
  case $1 in
    --fleet) fleet=$2; shift 2 ;;
    --ttl) ttl=$2; shift 2 ;;
    *) break ;;
  esac
done

case $fleet in
  ivan) manifest_file=t3code-fleet.nix secret=t3code-fleet-enrollment.age ssh_user=imalison ;;
  alex) manifest_file=t3code-fleet.alex.nix secret=t3code-fleet-enrollment.alex.age ssh_user=alex ;;
  *) echo "unknown fleet: $fleet" >&2; exit 1 ;;
esac
output="$root/nixos/secrets/$secret"

manifest=$(nix eval --json -f "$root/nix-shared/$manifest_file")
if [ $# -gt 0 ]; then
  hosts=("$@")
else
  mapfile -t hosts < <(jq -r '.[].label' <<<"$manifest")
fi
recipients=$(nix eval --json -f "$root/nixos/secrets/secrets.nix" "\"$secret\".publicKeys" | jq -r '.[]')

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
T3CODE_ENVIRONMENT_ID="$ENVIRONMENT_ID" "$t3" auth enrollment export \
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
    { echo "$host is not in nix-shared/$manifest_file" >&2; exit 1; }
  environment_id=$(jq -r --arg h "$host" '.[] | select(.label == $h) | .environmentId' <<<"$manifest")
  ssh_host=$(jq -r --arg h "$host" '.[] | select(.label == $h) | .httpBaseUrl | capture("^[a-z]+://(?<h>[^:/]+)").h' <<<"$manifest")
  echo "minting on $host" >&2
  { printf '%s\n' "$ephemeral"; jq -c '{version: 1, environments: .}' <<<"$manifest"; } |
    ssh -o BatchMode=yes "$ssh_user@$ssh_host" \
      "ENVIRONMENT_ID=$(printf %q "$environment_id") LABEL=$(printf %q "$label") TTL=$(printf %q "$ttl") bash -c $(printf %q "$remote")" \
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
