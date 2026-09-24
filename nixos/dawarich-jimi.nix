{
  config,
  lib,
  pkgs,
  ...
}: {
  age.secrets.dawarich-secret-key-base.file = ./secrets/dawarich-secret-key-base.age;

  services.dawarich = {
    enable = true;
    # The public Photon endpoint is unsuitable for a bulk sweep of imported points.
    package = pkgs.dawarich.overrideAttrs (old: {
      postPatch = (old.postPatch or "") + ''
        sed -i '/^nightly_reverse_geocoding_job:/,/^$/d' config/schedule.yml
      '';
    });
    configureNginx = false;
    localDomain = "jimi-hendnix";
    webPort = 47863;
    secretKeyBaseFile = config.age.secrets.dawarich-secret-key-base.path;
    environment = {
      BINDING = "100.114.206.79";
      APPLICATION_HOSTS = "jimi-hendnix,jimi-hendnix.taileb3aad.ts.net,100.114.206.79";
      ALLOW_EMAIL_PASSWORD_REGISTRATION = "false";
      PHOTON_API_HOST = "photon.komoot.io";
      PHOTON_API_USE_HTTPS = "true";
    };
  };

  networking.firewall.interfaces.tailscale0.allowedTCPPorts = [47863];
  systemd.services.dawarich-web = {
    wants = ["tailscaled.service"];
    after = ["tailscaled.service"];
  };

  systemd.services.dawarich-serve = {
    description = "Tailscale HTTPS for Dawarich";
    after = ["tailscaled.service" "dawarich-web.service"];
    wants = ["tailscaled.service" "dawarich-web.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = "60s";
      ExecStart = "${config.services.tailscale.package}/bin/tailscale serve --bg --https=443 --set-path=/ http://100.114.206.79:47863";
    };
  };

  # Keep reference-data seeds without creating upstream's default demo account.
  systemd.services.dawarich-init-db.script = lib.mkForce ''
    export SECRET_KEY_BASE="$(systemd-creds cat SECRET_KEY_BASE)"
    rails db:migrate
    rake data:migrate
    rails runner 'User.define_singleton_method(:none?) { false }; load Rails.root.join("db/seeds.rb")'
  '';
}
