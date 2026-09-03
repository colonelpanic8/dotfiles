# Merge declarative daemon settings into Paseo's config.json before the service
# starts.
#
# Paseo owns config.json at runtime: the app and CLI write to it, and it holds
# state Nix has no business generating. So this is a merge, not a template --
# declared keys win, everything else survives. Objects merge recursively;
# arrays are replaced whole, which is what you want for a declared list like
# liveVoice.contextProfiles.
{
  pkgs,
  settings,
}: let
  settingsFile = pkgs.writeText "paseo-daemon-settings.json" (builtins.toJSON settings);
in
  pkgs.writeShellScript "ensure-paseo-daemon-settings" ''
    set -eu

    config_file="$1"
    config_dir="$(${pkgs.coreutils}/bin/dirname "$config_file")"
    mkdir -p "$config_dir"
    temporary="$(${pkgs.coreutils}/bin/mktemp "$config_dir/.config.json.XXXXXX")"
    trap 'rm -f "$temporary"' EXIT

    if [ -f "$config_file" ]; then
      ${pkgs.jq}/bin/jq --slurpfile desired ${settingsFile} \
        '. * $desired[0]' "$config_file" > "$temporary"
    else
      ${pkgs.jq}/bin/jq --slurpfile desired ${settingsFile} -n \
        '{version: 1} * $desired[0]' > "$temporary"
    fi

    chmod 0600 "$temporary"
    mv "$temporary" "$config_file"
    trap - EXIT
  ''
