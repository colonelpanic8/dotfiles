{pkgs}:
pkgs.writeShellScript "ensure-paseo-mcp-injection" ''
  set -eu

  config_file="$1"
  config_dir="$(${pkgs.coreutils}/bin/dirname "$config_file")"
  mkdir -p "$config_dir"
  temporary="$(${pkgs.coreutils}/bin/mktemp "$config_dir/.config.json.XXXXXX")"
  trap 'rm -f "$temporary"' EXIT

  if [ -f "$config_file" ]; then
    ${pkgs.jq}/bin/jq '.daemon.mcp.injectIntoAgents = true' "$config_file" > "$temporary"
  else
    ${pkgs.jq}/bin/jq -n '{version: 1, daemon: {mcp: {injectIntoAgents: true}}}' > "$temporary"
  fi

  chmod 0600 "$temporary"
  mv "$temporary" "$config_file"
  trap - EXIT
''
