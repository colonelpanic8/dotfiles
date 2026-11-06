# Writes a T3 Code server's managed peer links file (T3CODE_MANAGED_PEER_LINKS_FILE)
# from the decrypted fleet enrollment bundle: one link per host in `hosts`, whose
# pairing code is that host's reusable enrollment credential. The credentials
# never reach the store. Without a readable bundle nothing is written, and the
# server leaves its links as they are.
{
  pkgs,
  lib,
  hosts,
  access,
}: enrollment: out:
pkgs.writeShellScript "t3code-write-peer-links" ''
  set -eu
  out=${out}
  [ -r ${lib.escapeShellArg enrollment} ] || exit 0
  umask 077
  ${pkgs.coreutils}/bin/mkdir -p "$(${pkgs.coreutils}/bin/dirname "$out")"
  ${pkgs.jq}/bin/jq \
    --argjson hosts ${lib.escapeShellArg (builtins.toJSON hosts)} \
    --arg access ${lib.escapeShellArg access} \
    '{version: 1, links: [.environments[] | select(.label as $host | $hosts | index($host))
      | {environmentId, label, urls: [.httpBaseUrl], access: $access, pairingCode: .credential}]}' \
    ${lib.escapeShellArg enrollment} > "$out.tmp"
  ${pkgs.coreutils}/bin/mv "$out.tmp" "$out"
''
