{authKeyFile ? null}: {
  config,
  lib,
  pkgs,
  ...
}: let
  tailscale = "${config.services.tailscale.package}/bin/tailscale";
  primaryUser = config.system.primaryUser;
in {
  services.tailscale.enable = true;
  launchd.daemons.tailscaled.serviceConfig.KeepAlive = true;

  # Authentication is optional and separate from the daemon/preferences.
  # Hosts without a key enroll once with `sudo tailscale up`.
  age.secrets = lib.optionalAttrs (authKeyFile != null) {
    tailscale-authkey = {
      file = authKeyFile;
      owner = "root";
      mode = "0400";
    };
  };

  # The GUI client and this daemon have independent identities and VPNs.
  # Require the documented one-time migration before starting another client.
  system.activationScripts.preActivation.text = lib.mkBefore ''
    if [ -d /Applications/Tailscale.app ]; then
      echo >&2 "Remove Tailscale.app, empty the Trash, and reboot before switching to the Nix-managed Tailscale daemon."
      echo >&2 "See nix-darwin/README.md for migration and login instructions."
      exit 1
    fi
  '';

  launchd.daemons.tailscale-autoconnect = {
    script = ''
      set -euo pipefail

      state=""
      for _ in $(${pkgs.coreutils}/bin/seq 1 30); do
        state="$(${tailscale} status --json 2>/dev/null | ${pkgs.jq}/bin/jq -r '.BackendState // empty' || true)"
        if [ -n "$state" ] && [ "$state" != "NoState" ]; then
          break
        fi
        sleep 2
      done
      if [ -z "$state" ] || [ "$state" = "NoState" ]; then
        echo "Timed out waiting for tailscaled" >&2
        exit 1
      fi

      # `set` changes only these preferences and never initiates a login.
      # Reconcile them even when authentication is handled manually.
      ${tailscale} set \
        --accept-dns=true \
        --hostname=${lib.escapeShellArg config.networking.hostName} \
        --operator=${lib.escapeShellArg primaryUser}

      ${lib.optionalString (authKeyFile != null) ''
        if [ "$state" = "Running" ]; then
          exit 0
        fi

        key_file=${lib.escapeShellArg config.age.secrets.tailscale-authkey.path}
        if [ ! -s "$key_file" ] || [ "$(cat "$key_file")" = "DISABLED" ]; then
          exit 0
        fi

        ${tailscale} up \
          --auth-key "file:$key_file" \
          --accept-dns=true \
          --hostname=${lib.escapeShellArg config.networking.hostName} \
          --operator=${lib.escapeShellArg primaryUser} \
          --timeout=60s
      ''}
    '';
    serviceConfig = {
      RunAtLoad = true;
      StartInterval = 300;
      StandardOutPath = "/var/log/tailscale-autoconnect.log";
      StandardErrorPath = "/var/log/tailscale-autoconnect.err.log";
    };
  };
}
