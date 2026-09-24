{
  config,
  lib,
  pkgs,
  ...
}: let
  photonJar = pkgs.fetchurl {
    url = "https://github.com/komoot/photon/releases/download/1.3.0/photon-1.3.0.jar";
    hash = "sha256-qJcHwAReSAeyoRgOEy5o4QjZmHCfSLbJS5im4oH1caU=";
  };
  photonDataDir = "/var/lib/photon-geocoder";
in {
  users.users.photon-geocoder = {
    isSystemUser = true;
    group = "photon-geocoder";
  };
  users.groups.photon-geocoder = {};

  systemd.services.photon-geocoder-database = {
    description = "Download the Photon planet search index";
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      User = "photon-geocoder";
      Group = "photon-geocoder";
      StateDirectory = "photon-geocoder";
      RemainAfterExit = true;
      TimeoutStartSec = "infinity";
    };
    script = ''
      set -euo pipefail
      cd ${photonDataDir}
      if test -d photon_data; then
        exit 0
      fi

      ${pkgs.curl}/bin/curl --fail --location --retry 5 --continue-at - \
        --output photon-db-planet-release-260920.tar.bz2 \
        https://download1.graphhopper.com/public/photon-db-planet-release-260920.tar.bz2
      printf '%s\n' '0dd94c1732920d0e560b40e7841a343e  photon-db-planet-release-260920.tar.bz2' \
        | ${pkgs.coreutils}/bin/md5sum --check
      rm -rf staging
      mkdir staging
      ${pkgs.pbzip2}/bin/pbzip2 -dc photon-db-planet-release-260920.tar.bz2 \
        | ${pkgs.gnutar}/bin/tar -xf - -C staging --no-same-owner
      mv staging/photon_data photon_data
      rmdir staging
      rm photon-db-planet-release-260920.tar.bz2
    '';
  };

  systemd.services.photon-geocoder = {
    description = "Local Photon geocoder for Dawarich";
    wantedBy = ["multi-user.target"];
    requires = ["photon-geocoder-database.service"];
    after = ["photon-geocoder-database.service"];
    serviceConfig = {
      User = "photon-geocoder";
      Group = "photon-geocoder";
      StateDirectory = "photon-geocoder";
      WorkingDirectory = photonDataDir;
      ExecStart = "${pkgs.jdk21_headless}/bin/java -Xmx8g -jar ${photonJar} serve -listen-ip 127.0.0.1 -listen-port 2323";
      Restart = "on-failure";
      MemoryMax = "12G";
      LimitNOFILE = 65536;
      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
    };
  };

  age.secrets.dawarich-secret-key-base.file = ./secrets/dawarich-secret-key-base.age;

  services.dawarich = {
    enable = true;
    configureNginx = false;
    localDomain = "jimi-hendnix";
    webPort = 47863;
    secretKeyBaseFile = config.age.secrets.dawarich-secret-key-base.path;
    environment = {
      BINDING = "100.114.206.79";
      APPLICATION_HOSTS = "jimi-hendnix,jimi-hendnix.taileb3aad.ts.net,100.114.206.79";
      ALLOW_EMAIL_PASSWORD_REGISTRATION = "false";
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
