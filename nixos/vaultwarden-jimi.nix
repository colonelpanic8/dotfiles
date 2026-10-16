{
  config,
  lib,
  pkgs,
  ...
}: let
  backendPort = 8221;
  httpsPort = 8222;
  bitwardenCli = pkgs.callPackage ./packages/bitwarden-cli-vaultwarden {};
  syncCommand = pkgs.writeShellApplication {
    name = "pass-vaultwarden-sync";
    runtimeInputs = [pkgs.pass pkgs.gnupg pkgs.git pkgs.coreutils];
    text = ''
      exec ${pkgs.python3}/bin/python3 ${../dotfiles/lib/bin/pass-vaultwarden-sync.py} --bw-command ${lib.getExe bitwardenCli} "$@"
    '';
  };
in {
  system.build.pass-vaultwarden-sync = syncCommand;
  assertions = [
    {
      assertion = config.networking.hostName == "jimi-hendnix";
      message = "vaultwarden-jimi.nix must only be imported on jimi-hendnix";
    }
  ];
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

  home-manager.users.imalison = {
    home.packages = [syncCommand];
    systemd.user.services.pass-vaultwarden-sync = {
      Unit = {
        Description = "Sync pass entries into the personal Vaultwarden vault";
        After = ["agenix.service" "import-gpg-key.service"];
      };
      Service = {
        Type = "oneshot";
        Environment = [
          "PASS_SYNC_GPG_PASSPHRASE_FILE=${builtins.replaceStrings ["$XDG_RUNTIME_DIR" "\${XDG_RUNTIME_DIR}"] ["%t" "%t"] config.home-manager.users.imalison.age.secrets.gpg-passphrase.path}"
        ];
        ExecStart = "${lib.getExe syncCommand} --apply";
        TimeoutStartSec = "45min";
        UMask = "0077";
      };
    };
    systemd.user.timers.pass-vaultwarden-sync = {
      Unit.Description = "Refresh Vaultwarden from pass every 15 minutes";
      Timer = {
        OnBootSec = "5min";
        OnUnitInactiveSec = "15min";
      };
      Install.WantedBy = ["timers.target"];
    };
  };
}
