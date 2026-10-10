{
  config,
  pkgs,
  ...
}: let
  backendPort = 8221;
  httpsPort = 8222;
in {
  age.secrets.vaultwarden-environment-file = {
    file = ./secrets/vaultwarden-environment-file.age;
    owner = "vaultwarden";
  };

  services.vaultwarden = {
    enable = true;
    backupDir = "/var/backup/vaultwarden";
    environmentFile = config.age.secrets.vaultwarden-environment-file.path;
    config = {
      DOMAIN = "https://jimi-hendnix.taileb3aad.ts.net:${toString httpsPort}";
      ROCKET_ADDRESS = "127.0.0.1";
      ROCKET_PORT = backendPort;
      SIGNUPS_ALLOWED = false;
    };
  };

  systemd.services.vaultwarden-serve = {
    description = "Tailscale HTTPS for Vaultwarden";
    after = ["tailscaled.service" "vaultwarden.service"];
    wants = ["tailscaled.service" "vaultwarden.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = "60s";
      ExecStartPre = "${config.services.tailscale.package}/bin/tailscale wait --timeout=45s";
      ExecStart = "${pkgs.util-linux}/bin/flock /run/tailscale-serve.lock ${config.services.tailscale.package}/bin/tailscale serve --bg --https=${toString httpsPort} --set-path=/ http://127.0.0.1:${toString backendPort}";
      Restart = "on-failure";
      RestartSec = "5s";
    };
  };
}
