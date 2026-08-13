{
  config,
  lib,
  pkgs,
  makeEnable,
  ...
}:
makeEnable config "myModules.tailscale" true {
  # Provide stable SSH connectivity between your machines without needing port
  # forwarding (works behind NAT/CGNAT).
  services.tailscale.enable = true;

  # Handy even if you only enable the service and run `tailscale up` manually.
  environment.systemPackages = [pkgs.tailscale];

  # Optional: unattended enrollment using a pre-auth key stored in agenix.
  #
  # Plaintext content "DISABLED" means "do nothing".
  #
  # This secret is encrypted to your "kanivan" SSH keys, so we include your
  # user SSH key as an identity for decryption.
  age.identityPaths = lib.mkDefault [
    "/etc/ssh/ssh_host_ed25519_key"
    "/home/imalison/.ssh/id_ed25519"
  ];

  age.secrets.tailscale-authkey = {
    file = ./secrets/tailscale-authkey.age;
    owner = "root";
    group = "root";
    mode = "0400";
  };

  systemd.services.tailscale-autoconnect = {
    description = "Auto-connect Tailscale (optional, via agenix auth key)";
    wantedBy = ["multi-user.target"];
    after = ["network-online.target" "tailscaled.service"];
    wants = ["network-online.target" "tailscaled.service"];

    unitConfig = {
      ConditionPathExists = config.age.secrets.tailscale-authkey.path;
    };

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = false;
    };

    script = ''
      set -euo pipefail

      key_file='${config.age.secrets.tailscale-authkey.path}'
      if [ ! -s "$key_file" ]; then
        exit 0
      fi
      if [ "$(cat "$key_file")" = "DISABLED" ]; then
        exit 0
      fi

      status_json="$(${pkgs.tailscale}/bin/tailscale status --json 2>/dev/null || true)"
      state="$(printf '%s' "$status_json" | ${pkgs.jq}/bin/jq -r '.BackendState // empty' 2>/dev/null || true)"
      key_expiry="$(printf '%s' "$status_json" | ${pkgs.jq}/bin/jq -r '.Self.KeyExpiry // empty' 2>/dev/null || true)"
      key_expired=false
      if [ -n "$key_expiry" ]; then
        expiry_epoch="$(${pkgs.coreutils}/bin/date -d "$key_expiry" +%s 2>/dev/null || true)"
        now_epoch="$(${pkgs.coreutils}/bin/date +%s)"
        if [ -n "$expiry_epoch" ] && [ "$expiry_epoch" -le "$now_epoch" ]; then
          key_expired=true
        fi
      fi

      if [ "$state" = "Running" ] && [ "$key_expired" = false ]; then
        exit 0
      fi

      reauth_args=()
      if [ "$key_expired" = true ]; then
        reauth_args+=(--force-reauth)
      fi

      if ! ${pkgs.tailscale}/bin/tailscale up \
        "''${reauth_args[@]}" \
        --auth-key "file:$key_file" \
        --accept-dns=true \
        --hostname=${lib.escapeShellArg config.networking.hostName} \
        --operator=imalison \
        --timeout=60s; then
        echo "tailscale-autoconnect: tailscale up failed; leaving manual login required" >&2
        exit 0
      fi
    '';
  };

  systemd.timers.tailscale-autoconnect = {
    description = "Periodically repair Tailscale authentication";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnBootSec = "2m";
      OnUnitActiveSec = "30m";
      Persistent = true;
    };
  };
}
