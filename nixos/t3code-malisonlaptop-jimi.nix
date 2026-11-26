{
  config,
  lib,
  pkgs,
  ...
}: let
  keys = import ../nix-shared/keys.nix;
  family = import ../nix-shared/t3code-family.nix;
  msiHost = "msi-elliott.tail40f15e.ts.net";
  windowsHost = "malisonlaptop.tail40f15e.ts.net";
  localPort = "13775";
  httpsPort = builtins.head (builtins.match "https://[^:]+:([0-9]+)/" family.alex.hosts.malisonlaptop.url);
  knownHosts = pkgs.writeText "t3code-malisonlaptop-known-hosts" "${msiHost} ${keys.msiElliottHostKey}\n";
  tailscale = "${config.services.tailscale.package}/bin/tailscale";
  serve = "${pkgs.util-linux}/bin/flock /run/tailscale-serve.lock ${tailscale} serve";
in {
  systemd.services.t3code-malisonlaptop-tunnel = {
    description = "SSH tunnel through msi-elliott to MalisonLaptop";
    after = ["tailscaled.service"];
    wants = ["tailscaled.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      User = "imalison";
      ExecStart = lib.escapeShellArgs [
        "${pkgs.openssh}/bin/ssh"
        "-F"
        "/dev/null"
        "-NT"
        "-o"
        "BatchMode=yes"
        "-o"
        "ExitOnForwardFailure=yes"
        "-o"
        "ConnectTimeout=10"
        "-o"
        "ServerAliveInterval=30"
        "-o"
        "ServerAliveCountMax=3"
        "-o"
        "StrictHostKeyChecking=yes"
        "-o"
        "UserKnownHostsFile=${knownHosts}"
        "-o"
        "IdentitiesOnly=yes"
        "-i"
        "${config.users.users.imalison.home}/.ssh/id_ed25519"
        "-L"
        "127.0.0.1:${localPort}:${windowsHost}:3773"
        "alex@${msiHost}"
      ];
      Restart = "always";
      RestartSec = "5s";
    };
  };

  systemd.services.t3code-malisonlaptop-serve = {
    description = "Tailscale HTTPS for MalisonLaptop T3 Code";
    after = ["tailscaled.service" "t3code-malisonlaptop-tunnel.service"];
    wants = ["tailscaled.service" "t3code-malisonlaptop-tunnel.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = "60s";
      ExecStartPre = "${tailscale} wait --timeout=45s";
      ExecStart = "${serve} --bg --https=${httpsPort} --set-path=/ http://127.0.0.1:${localPort}";
      ExecStop = "${serve} --https=${httpsPort} --set-path=/ off";
      Restart = "on-failure";
      RestartSec = "5s";
    };
  };
}
