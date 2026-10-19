#!/usr/bin/env bash
# Seals nix-shared/t3code-mobile-settings.nix to the fleet enrollment
# recipients at nixos/secrets/t3code-mobile-settings.age; build.sh publishes it.
set -euo pipefail

if ! command -v age >/dev/null || ! command -v jq >/dev/null; then
  exec nix shell nixpkgs#age nixpkgs#jq --command "$0" "$@"
fi

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
output="$root/nixos/secrets/t3code-mobile-settings.age"

recipients=$(nix eval --json -f "$root/nixos/secrets/secrets.nix" '"t3code-mobile-settings.age".publicKeys' | jq -r '.[]')
args=()
while IFS= read -r recipient; do args+=(-r "$recipient"); done <<<"$recipients"

nix eval --json -f "$root/nix-shared/t3code-mobile-settings.nix" | age --armor "${args[@]}" -o "$output"
echo "wrote $output"
