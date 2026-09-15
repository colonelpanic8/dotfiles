# google-messages.nix - Google Messages multi-device bridge.
#
# The desktop client runs everywhere; the bridge server runs on exactly one
# host, because it owns the bbolt database and the single Google connection.
# Both halves read their secrets from agenix files rather than `pass`: the GPG
# key is passphrase-protected, so a `pass`-backed unit would fail-loop after a
# reboot until someone unlocked the agent.
{
  config,
  inputs,
  lib,
  pkgs,
  makeEnable,
  ...
}:
let
  # The host that owns the database. Two servers on one database would fight
  # over the lock and the Google connection.
  bridgeHost = "jimi-hendnix";
  isBridgeHost = config.networking.hostName == bridgeHost;
  # Loopback only; Tailscale Serve terminates TLS and proxies in.
  bridgePort = 45012;
  servePort = 8443;
  pairingHelperManifest = builtins.fromJSON (
    builtins.readFile "${inputs.google-messages-bridge}/internal/api/pairinghelper/manifest.json"
  );
  pairingHelperId = "epobdoolljeefagfcdolcadallpopeon";
in
makeEnable config "myModules.googleMessages" true {
  imports = [ inputs.google-messages-bridge.nixosModules.default ];

  # Encrypted to every host (keys.agenixKeys) so the client unlocks without
  # typing anything on any machine.
  age.secrets.google-messages-bridge-api-token = {
    file = ./secrets/google-messages-bridge-api-token.age;
    owner = "imalison";
    group = "users";
    mode = "0400";
  };

  age.secrets.google-messages-bridge-storage-key = lib.mkIf isBridgeHost {
    file = ./secrets/google-messages-bridge-storage-key.age;
    owner = "imalison";
    group = "users";
    mode = "0400";
  };

  services.google-messages-multidevice-bridge.client = {
    enable = true;
    bridgeUrl = "https://${bridgeHost}.taileb3aad.ts.net:${toString servePort}";
    apiTokenFile = config.age.secrets.google-messages-bridge-api-token.path;
  };

  # The bridge is a user service, so it only comes back after a reboot if the
  # user session outlives logout.
  users.users.imalison.linger = lib.mkIf isBridgeHost true;

  home-manager.users.imalison = {
    imports = [ inputs.google-messages-bridge.homeManagerModules.default ];

    services.google-messages-multidevice-bridge = lib.mkIf isBridgeHost {
      enable = true;
      package = pkgs.google-messages-multidevice-bridge;
      listen = "127.0.0.1:${toString bridgePort}";
      storageKeyFile = config.age.secrets.google-messages-bridge-storage-key.path;
      apiTokenFile = config.age.secrets.google-messages-bridge-api-token.path;
    };

    xdg.configFile."google-chrome/External Extensions/${pairingHelperId}.json" = lib.mkIf isBridgeHost {
      text = builtins.toJSON {
        external_crx = "${./assets/google-messages-pairing-helper.crx}";
        external_version = pairingHelperManifest.version;
      };
    };
  };

  # Serve config is node-wide state owned by root, and re-applying it is
  # idempotent, so this also repairs the mapping if tailscaled state is reset.
  systemd.services.google-messages-bridge-serve = lib.mkIf isBridgeHost {
    description = "Tailscale Serve mapping for the Google Messages bridge";
    after = [ "tailscaled.service" ];
    wants = [ "tailscaled.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${config.services.tailscale.package}/bin/tailscale serve --bg --https=${toString servePort} --set-path=/ http://127.0.0.1:${toString bridgePort}";
    };
  };
}
