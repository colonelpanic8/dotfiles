# Defaults and managed provider settings shared by NixOS and macOS fleet members.
{
  config,
  lib,
  pkgs,
  ...
}: let
  antigravityInstanceId = "antigravity";
  t3DataDirectory = config.services.t3code.dataDirectory or "${config.home.homeDirectory}/.t3";
  antigravityProfile = "${t3DataDirectory}/userdata/providers/antigravity/${builtins.hashString "sha256" antigravityInstanceId}";
in {
  services.t3code = {
    sharedSettings = builtins.fromJSON (builtins.readFile ../t3code-settings/defaults.json);
    settings = {
      providerInstances.claudeAgent = lib.mkDefault {
        driver = "claudeAgent";
        config.launchArgs = "--chrome";
      };
      # Gemini, now served through Antigravity. Pinning the runtime keeps T3
      # from downloading its own ~1 GB copy into ~/.t3/tools on every host.
      providerInstances.antigravity = lib.mkIf (lib.meta.availableOn pkgs.stdenv.hostPlatform pkgs.antigravity-acp) {
        driver = "antigravity";
        displayName = "Antigravity";
        enabled = true;
        config.binaryPath = "${pkgs.antigravity-acp}/bin/agy_acp_server.par";
      };
    };
    keybindings = builtins.fromJSON (builtins.readFile ../t3code-keybindings.json);
    fleetManifest = import ../t3code-fleet.nix;
  };

  # T3 Code isolates Antigravity under its own GEMINI_HOME profile; mirror the
  # user's ~/.gemini skill directories there (replacing any empty directories
  # created on first launch).
  home.activation.linkT3CodeAntigravitySkills = lib.hm.dag.entryAfter ["writeBoundary"] ''
    gemini_home=${lib.escapeShellArg "${config.home.homeDirectory}/.gemini"}
    profile_dir=${lib.escapeShellArg antigravityProfile}

    for subdir in config/skills antigravity-cli/skills; do
      target="$gemini_home/$subdir"
      link="$profile_dir/$subdir"

      mkdir -p "$(dirname "$link")"
      if [ -L "$link" ]; then
        rm -f "$link"
      elif [ -d "$link" ]; then
        if ! rmdir "$link" 2>/dev/null; then
          echo "Skipping $link: non-empty directory exists" >&2
          continue
        fi
      elif [ -e "$link" ]; then
        echo "Skipping $link: non-directory entry exists" >&2
        continue
      fi

      ln -s "$target" "$link"
    done
  '';
}
