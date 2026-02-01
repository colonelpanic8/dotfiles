# org-agenda-api-host.nix - Host org-agenda-api container with nginx + Let's Encrypt
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.org-agenda-api-host;
  # Random high port to avoid conflicts
  containerPort = 51847;
in
{
  options.services.org-agenda-api-host = {
    enable = mkEnableOption "org-agenda-api container hosting";

    domain = mkOption {
      type = types.str;
      default = "rbsf.tplinkdns.com";
      description = "Base domain name (service will be at org-agenda-api.<domain>)";
    };

    acmeEmail = mkOption {
      type = types.str;
      default = "IvanMalison@gmail.com";
      description = "Email for Let's Encrypt certificate notifications";
    };

    containerImageFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Nix-built container image (tarball from dockerTools)";
    };

    containerImage = mkOption {
      type = types.str;
      default = "colonelpanic-org-agenda-api";
      description = "Container image name (used when imageFile is provided)";
    };

    gitSyncRepository = mkOption {
      type = types.str;
      default = "git@github.com:colonelpanic8/org.git";
      description = "Git repository to sync org files from";
    };

    gitUserEmail = mkOption {
      type = types.str;
      default = "IvanMalison@gmail.com";
      description = "Git user email for commits";
    };

    gitUserName = mkOption {
      type = types.str;
      default = "Ivan Malison";
      description = "Git user name for commits";
    };

    authUser = mkOption {
      type = types.str;
      default = "imalison";
      description = "Basic auth username";
    };

    secretsFile = mkOption {
      type = types.path;
      description = "Path to agenix-decrypted secrets file containing AUTH_PASSWORD";
    };

    sshKeyFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Path to agenix-decrypted SSH private key file (mounted at /secrets/ssh_key in container)";
    };

    timezone = mkOption {
      type = types.str;
      default = "America/Los_Angeles";
      description = "Timezone for the container";
    };
  };

  config = mkIf cfg.enable {
    # Enable ACME for Let's Encrypt
    security.acme = {
      acceptTerms = true;
      defaults.email = cfg.acmeEmail;
    };

    # Nginx reverse proxy with TLS
    services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;

      virtualHosts."org-agenda-api.${cfg.domain}" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString containerPort}";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_read_timeout 300s;
            proxy_connect_timeout 75s;
          '';
        };
      };
    };

    # Open firewall for HTTP/HTTPS
    networking.firewall.allowedTCPPorts = [ 80 443 ];

    # Container service using podman
    virtualisation.oci-containers = {
      backend = "podman";
      containers.org-agenda-api = {
        image = cfg.containerImage;
        imageFile = cfg.containerImageFile;
        autoStart = true;
        ports = [ "127.0.0.1:${toString containerPort}:80" ];
        environment = {
          TZ = cfg.timezone;
          GIT_SYNC_REPOSITORY = cfg.gitSyncRepository;
          GIT_USER_EMAIL = cfg.gitUserEmail;
          GIT_USER_NAME = cfg.gitUserName;
          AUTH_USER = cfg.authUser;
        };
        environmentFiles = [ cfg.secretsFile ];
        volumes = lib.optionals (cfg.sshKeyFile != null) [
          "${cfg.sshKeyFile}:/secrets/ssh_key:ro"
        ];
        extraOptions = [
          "--pull=never"  # Image is from nix store, don't try to pull
        ];
      };
    };

    # Ensure container restarts on failure
    systemd.services.podman-org-agenda-api = {
      serviceConfig = {
        Restart = "always";
        RestartSec = "10s";
      };
    };
  };
}
