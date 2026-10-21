{
  config,
  pkgs,
  ...
}: let
  lanAddress = "192.168.0.214";
  backendPort = 48763;
  tlsPort = 45443;
  image = "railbird-sf:5279/rocket-sense-server@sha256:840daa2cb3370fd30836314d8bec3a62a146a4bce1f3acf0a68695b2c545bf95";
  environment = {
    ROCKET_SENSE_LOG_FORMAT = "json";
    ROCKET_SENSE_STORAGE_ROOT = "/var/lib/rocket-sense/storage";
    ROCKET_SENSE_PUBLIC_BASE_URL = "https://rocket-sense.duckdns.org";
    ROCKET_SENSE_RANK_BENCHMARK_WINDOWS = "rolling-12m";
  };
  appContainer = {
    inherit image;
    environmentFiles = [config.age.secrets.rocket-sense-environment.path];
    volumes = ["/var/lib/rocket-sense/storage:/var/lib/rocket-sense/storage"];
    extraOptions = ["--network=rocket-sense" "--pull=never"];
    dependsOn = ["rocket-sense-postgres"];
  };
in {
  age.secrets.rocket-sense-environment.file = ./secrets/rocket-sense.env.age;

  systemd.tmpfiles.rules = [
    "d /var/lib/rocket-sense 0700 root root -"
    "d /var/lib/rocket-sense/postgres 0755 root root -"
  ];
  systemd.services.rocket-sense-network = {
    description = "Rocket Sense container network";
    before = ["podman-rocket-sense-postgres.service"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      ${pkgs.podman}/bin/podman network exists rocket-sense || ${pkgs.podman}/bin/podman network create rocket-sense
    '';
  };

  virtualisation.oci-containers = {
    backend = "podman";
    containers = {
      rocket-sense-postgres = {
        image = "docker.io/library/postgres:16-alpine";
        cmd = ["-c" "client_connection_check_interval=10s"];
        environment = {
          POSTGRES_DB = "rocket_sense";
          POSTGRES_USER = "rocket_sense";
          PGDATA = "/var/lib/postgresql/data/pgdata";
        };
        environmentFiles = [config.age.secrets.rocket-sense-environment.path];
        volumes = ["/var/lib/rocket-sense/postgres:/var/lib/postgresql/data"];
        extraOptions = [
          "--network=rocket-sense"
          "--network-alias=rocket-sense-postgres.rocket-sense.svc.cluster.local"
          "--pull=never"
        ];
      };
      rocket-sense =
        appContainer
        // {
          ports = ["127.0.0.1:${toString backendPort}:8080"];
          extraOptions = appContainer.extraOptions ++ ["--memory=4g"];
          environment =
            environment
            // {
              ROCKET_SENSE_BIND_ADDR = "0.0.0.0:8080";
              ROCKET_SENSE_SERVICE_MODE = "server";
              ROCKET_SENSE_MATERIALIZED_STAT_COUNTS = "true";
              ROCKET_SENSE_RANK_BENCHMARK = "true";
              ROCKET_SENSE_RUN_REPLAY_PROCESSING_WORKERS = "false";
            };
        };
      rocket-sense-worker =
        appContainer
        // {
          environment =
            environment
            // {
              ROCKET_SENSE_SERVICE_MODE = "worker";
              ROCKET_SENSE_BACKGROUND_PROCESSING_CONCURRENCY = "2";
              ROCKET_SENSE_RANK_BENCHMARK = "false";
            };
          extraOptions = appContainer.extraOptions ++ ["--memory=8g"];
        };
    };
  };

  systemd.services.podman-rocket-sense-postgres = {
    requires = ["rocket-sense-network.service"];
    after = ["rocket-sense-network.service"];
    unitConfig.ConditionPathExists = "/var/lib/rocket-sense/.migration-complete";
  };
  systemd.services.podman-rocket-sense.unitConfig.ConditionPathExists = "/var/lib/rocket-sense/.migration-complete";
  systemd.services.podman-rocket-sense-worker.unitConfig.ConditionPathExists = "/var/lib/rocket-sense/.migration-complete";
  systemd.services.podman-rocket-sense.serviceConfig.RestartSec = "5s";
  systemd.services.podman-rocket-sense-worker.serviceConfig.RestartSec = "5s";
  systemd.services."acme-order-renew-rocket-sense.duckdns.org".unitConfig.ConditionPathExists = "/var/lib/rocket-sense/.migration-complete";

  # DuckDNS token for the rocket-sense.duckdns.org dynamic-DNS updater below.
  age.secrets.duckdns-token = {
    file = ./secrets/duckdns-token.railbird-sf.age;
    mode = "0400";
  };

  # Dynamic-DNS updater: keep rocket-sense.duckdns.org pointed at this node's
  # current public IP. The residential WAN IP changes (e.g. after an ISP
  # outage/failover), and without this the public hostname goes stale and the
  # site becomes unreachable until manually re-pointed. Leaving ip= blank tells
  # DuckDNS to detect the source IP of this request, so it always reflects
  # whatever connection the node is currently egressing through.
  systemd.services.duckdns = {
    description = "DuckDNS updater for rocket-sense.duckdns.org";
    after = ["network-online.target"];
    wants = ["network-online.target"];
    serviceConfig = {
      Type = "oneshot";
      DynamicUser = true;
      LoadCredential = ["token:${config.age.secrets.duckdns-token.path}"];
    };
    script = ''
      token="$(cat "$CREDENTIALS_DIRECTORY/token")"
      resp="$(${pkgs.curl}/bin/curl -fsS --retry 3 --max-time 30 \
        "https://www.duckdns.org/update?domains=rocket-sense&token=$token&ip=")"
      echo "DuckDNS response: $resp"
      [ "$resp" = "OK" ] || { echo "DuckDNS update failed"; exit 1; }
    '';
  };

  systemd.timers.duckdns = {
    description = "Periodic DuckDNS update for rocket-sense.duckdns.org";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnBootSec = "1min";
      OnUnitActiveSec = "5min";
      Persistent = true;
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "IvanMalison@gmail.com";
  };
  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    recommendedGzipSettings = true;
    streamConfig = ''
      map $ssl_preread_server_name $https_backend {
        rocket-sense.duckdns.org 127.0.0.1:${toString tlsPort};
        default 127.0.0.1:45444;
      }
      server {
        listen ${lanAddress}:443;
        ssl_preread on;
        proxy_pass $https_backend;
        proxy_protocol on;
        proxy_connect_timeout 10s;
        proxy_timeout 300s;
      }
      server {
        listen 127.0.0.1:45444 proxy_protocol;
        proxy_pass 192.168.0.159:443;
        proxy_timeout 300s;
      }
    '';
    virtualHosts = {
      "rocket-sense.duckdns.org" = {
        enableACME = true;
        forceSSL = true;
        listen = [
          {
            addr = lanAddress;
            port = 80;
          }
          {
            addr = "127.0.0.1";
            port = tlsPort;
            ssl = true;
            extraParameters = ["proxy_protocol"];
          }
        ];
        extraConfig = ''
          set_real_ip_from 127.0.0.1;
          real_ip_header proxy_protocol;
        '';
        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString backendPort}";
          proxyWebsockets = true;
          extraConfig = ''
            client_max_body_size 200m;
            proxy_read_timeout 300s;
            proxy_connect_timeout 75s;
          '';
        };
        locations."= /privacy" = {
          alias = ./rocket-sense-privacy.html;
          extraConfig = "default_type text/html;";
        };
      };
      "_" = {
        default = true;
        listen = [
          {
            addr = lanAddress;
            port = 80;
          }
        ];
        locations."/".proxyPass = "http://192.168.0.159";
      };
    };
  };
  networking.firewall.allowedTCPPorts = [80 443];
}
