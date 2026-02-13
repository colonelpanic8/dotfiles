{ config, lib, pkgs, makeEnable, ... }:
makeEnable config "myModules.tailscale" true {
  # Provide stable SSH connectivity between your machines without needing port
  # forwarding (works behind NAT/CGNAT).
  services.tailscale.enable = true;

  # Handy even if you only enable the service and run `tailscale up` manually.
  environment.systemPackages = [ pkgs.tailscale ];

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
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" "tailscaled.service" "agenix.service" ];
    wants = [ "network-online.target" "tailscaled.service" ];

    unitConfig = {
      ConditionPathExists = config.age.secrets.tailscale-authkey.path;
    };

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
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

      state="$(${pkgs.tailscale}/bin/tailscale status --json 2>/dev/null | ${pkgs.jq}/bin/jq -r '.BackendState // empty' || true)"
      if [ "$state" = "Running" ]; then
        exit 0
      fi

      # First-time (or post-logout) login.
      ${pkgs.tailscale}/bin/tailscale up \\
        --auth-key "file:$key_file" \\
        --accept-dns=true \\
        --operator=imalison \\
        --timeout=60s
    '';
  };
}
