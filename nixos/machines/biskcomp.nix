{ pkgs, forEachUser, config, ... }:
let biskcomp-nginx-hostnames = "192.168.1.44 railbird.ai 1896Folsom.duckdns.org biskcomp.local 0.0.0.0 67.162.131.71";
in
{
  imports = [
    ../configuration.nix
    ../raspberry-pi.nix
  ];

  users.users.nginx = {
    group = "nginx";
    isSystemUser = true;
    extraGroups = ["syncthing"];
  };

  myModules.raspberry-pi.enable = true;

  myModules.base.enable = true;
  myModules.desktop.enable = true;
  myModules.xmonad.enable = false;
  myModules.extra.enable = false;
  myModules.code.enable = true;
  myModules.games.enable = false;
  myModules.syncthing.enable = true;
  myModules.fonts.enable = true;
  myModules.nixified-ai.enable = false;
  myModules.cache-server = {
    enable = false;
    host-string = biskcomp-nginx-hostnames;
    port = 80;
    path = "/nix-cache";
  };
  myModules.gitea.enable = true;
  myModules.gitea-runner.enable = false;

  myModules.railbird-k3s = {
    enable = false;
    serverAddr = "https://dev.railbird.ai:6443";
  };
  services.k3s.disableAgent = true;

  age.secrets.vaultwarden-environment-file = {
    file = ../secrets/vaultwarden-environment-file.age;
    owner = "vaultwarden";
  };

  services.vaultwarden = {
    enable = true;
    backupDir = "/var/backup/vaultwarden";
    environmentFile = config.age.secrets.vaultwarden-environment-file.path;
    config = {
      ROCKET_ADDRESS = "::1";
      ROCKET_PORT = 8222;
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "IvanMalison@gmail.com";
  };

  services.gitlab = {
    enable = true;
    databasePasswordFile = pkgs.writeText "dbPassword" "zgvcyfwsxzcwr85l";
    initialRootPasswordFile = pkgs.writeText "rootPassword" "dakqdvp4ovhksxer";
    host = "gitlab.railbird.ai";
    secrets = {
      secretFile = pkgs.writeText "secret" "Aig5zaic";
      otpFile = pkgs.writeText "otpsecret" "Riew9mue";
      dbFile = pkgs.writeText "dbsecret" "we2quaeZ";
      jwsFile = pkgs.runCommand "oidcKeyBase" {} "${pkgs.openssl}/bin/openssl genrsa 2048 > $out";
    };
  };

  age.secrets.discourse-admin-password = {
    file = ../secrets/discourse-admin-password.age;
    mode = "770";
    owner = "discourse";
    group = "users";
  };

  age.secrets.discourse-secret-key-base = {
    file = ../secrets/discourse-secret-key-base.age;
    group = "users";
    owner = "discourse";
  };

  services.discourse = {
    enable = false;
    enableACME = true;
    hostname = "discourse.railbird.ai";
    admin = {
      passwordFile = config.age.secrets.discourse-admin-password.path;
      email = "support@railbird.ai";
      fullName = "Admin";
      username = "admin";
    };
    secretKeyBaseFile = config.age.secrets.discourse-secret-key-base.path;
    database.ignorePostgresqlVersion = true;
  };

  systemd.services.emacs-org-api = {
    description = "Emacs org api";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Restart = "on-failure";
      User = "imalison";
    };
    ExecStart = "${pkgs.emacs} --load /home/imalison/org-api.el --daemon=org-api";
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedTlsSettings = true;
    virtualHosts = {
      "org-mode.1896folsom.duckdns.org" = {
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:2025";
      };
      "gitlab.railbird.ai" = {
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://unix:/run/gitlab/gitlab-workhorse.socket";
      };
      "vaultwarden.railbird.ai" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://[::1]:8222";
        };
      };
      "cache.railbird.ai" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://192.168.1.20:3090";
        };
      };
      "syncthing.railbird.ai" = {
        enableACME = true;
        forceSSL = true;
        root = "/var/lib/syncthing/railbird";
        locations."/" = {
          extraConfig = ''
            autoindex on;
          '';
        };
      };
      "docs.railbird.ai" = {
        enableACME = true;
        forceSSL = true;
        root = "/var/lib/syncthing/railbird/docs";
        locations."/" = {
          extraConfig = ''
            autoindex on;
          '';
        };
      };
    };
  };

  services.plex = {
    enable = true;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXOS_SD";
    fsType = "ext4";
  };

  services.home-assistant = {
    enable = false;
    extraComponents = [
      # Components required to complete the onboarding
      "met"
      "radio_browser"
    ];
    config = {
      # Includes dependencies for a basic setup
      # https://www.home-assistant.io/integrations/default_config/
      default_config = {};
    };
  };

  swapDevices = [
    { device = "/swapfile"; size = 8192; } # size is in MiB
  ];

  networking.hostName = "biskcomp";
  system.stateVersion = "23.11";

  home-manager.users = forEachUser {
    home.stateVersion = "23.11";
  };
}
