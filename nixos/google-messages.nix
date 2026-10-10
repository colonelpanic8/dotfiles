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
  # A second instance of the same bridge binary carries the WhatsApp linked
  # device. It owns its own database, secrets, port, and HTTPS origin, so the
  # two networks never share a lock or a token.
  whatsappPort = 45013;
  whatsappServePort = 8444;
  pairingHelperManifest = builtins.fromJSON (
    builtins.readFile "${inputs.google-messages-bridge}/internal/api/pairinghelper/manifest.json"
  );
  pairingHelperId = "epobdoolljeefagfcdolcadallpopeon";
  pairingHelperUpdateManifest = pkgs.writeText "google-messages-pairing-helper-updates.xml" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <gupdate xmlns="http://www.google.com/update2/response" protocol="2.0">
      <app appid="${pairingHelperId}">
        <updatecheck codebase="file://${./assets/google-messages-pairing-helper.crx}" version="${pairingHelperManifest.version}" />
      </app>
    </gupdate>
  '';
  pairingHelperPolicy = pkgs.writeText "google-messages-pairing-helper-policy.json" (
    builtins.toJSON {
      ExtensionInstallForcelist = [
        "${pairingHelperId};file://${pairingHelperUpdateManifest}"
      ];
      ExtensionSettings.${pairingHelperId} = {
        installation_mode = "force_installed";
        override_update_url = true;
        update_url = "file://${pairingHelperUpdateManifest}";
      };
    }
  );
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

  age.secrets.whatsapp-bridge-api-token = lib.mkIf isBridgeHost {
    file = ./secrets/whatsapp-bridge-api-token.age;
    owner = "imalison";
    group = "users";
    mode = "0400";
  };

  age.secrets.whatsapp-bridge-storage-key = lib.mkIf isBridgeHost {
    file = ./secrets/whatsapp-bridge-storage-key.age;
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
      instances.whatsapp = {
        enable = true;
        package = pkgs.google-messages-multidevice-bridge;
        network = "whatsapp";
        listen = "127.0.0.1:${toString whatsappPort}";
        storageKeyFile = config.age.secrets.whatsapp-bridge-storage-key.path;
        apiTokenFile = config.age.secrets.whatsapp-bridge-api-token.path;
      };
    };

  };

  environment.etc."opt/chrome/policies/managed/google-messages-pairing-helper.json" =
    lib.mkIf isBridgeHost
      {
        source = pairingHelperPolicy;
      };

  system.activationScripts.googleMessagesPairingHelperCleanup = lib.mkIf isBridgeHost {
    text = ''
      rm -f /opt/google/chrome/extensions/${pairingHelperId}.json
    '';
  };

  # Serve config is node-wide state owned by root, and re-applying it is
  # idempotent, so this also repairs the mapping if tailscaled state is reset.
  # Concurrent writers fail with an etag mismatch, hence the shared flock.
  systemd.services.google-messages-bridge-serve = lib.mkIf isBridgeHost {
    description = "Tailscale Serve mapping for the Google Messages bridge";
    after = [ "tailscaled.service" ];
    wants = [ "tailscaled.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = "60s";
      ExecStartPre = "${config.services.tailscale.package}/bin/tailscale wait --timeout=45s";
      ExecStart = "${pkgs.util-linux}/bin/flock /run/tailscale-serve.lock ${config.services.tailscale.package}/bin/tailscale serve --bg --https=${toString servePort} --set-path=/ http://127.0.0.1:${toString bridgePort}";
      Restart = "on-failure";
      RestartSec = "5s";
    };
  };

  systemd.services.whatsapp-bridge-serve = lib.mkIf isBridgeHost {
    description = "Tailscale Serve mapping for the WhatsApp bridge";
    after = [ "tailscaled.service" ];
    wants = [ "tailscaled.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = "60s";
      ExecStartPre = "${config.services.tailscale.package}/bin/tailscale wait --timeout=45s";
      ExecStart = "${pkgs.util-linux}/bin/flock /run/tailscale-serve.lock ${config.services.tailscale.package}/bin/tailscale serve --bg --https=${toString whatsappServePort} --set-path=/ http://127.0.0.1:${toString whatsappPort}";
      Restart = "on-failure";
      RestartSec = "5s";
    };
  };
}
