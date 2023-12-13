{ forEachUser, ... }:
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

  modules.raspberry-pi.enable = true;

  modules.base.enable = true;
  modules.desktop.enable = true;
  modules.xmonad.enable = false;
  modules.extra.enable = false;
  modules.code.enable = true;
  modules.games.enable = false;
  modules.syncthing.enable = true;
  modules.fonts.enable = true;
  modules.nixified-ai.enable = false;
  modules.cache-server = {
    enable = false;
    host-string = biskcomp-nginx-hostnames;
    port = 80;
    path = "/nix-cache";
  };
  modules.gitea.enable = true;
  modules.gitea-runner.enable = false;

  services.vaultwarden = {
    enable = true;
    config = {
      ROCKET_ADDRESS = "::1";
      ROCKET_PORT = 8222;
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "IvanMalison@gmail.com";
  };

  # services.nextcloud = {
  #   enable = true;
  #   hostName = "nextcloud.railbird.ai";
  #   config = {
  #     dbtype = "pgsql";
  #     database.createLocally = true;
  #   };
  # };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedTlsSettings = true;
    virtualHosts = {
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
          proxyPass = "http://192.168.1.26:3090";
        };
      };
      "syncthing.railbird.ai" = {
        enableACME = true;
        forceSSL = true;
        root = "/var/lib/syncthing/sync";
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
    enable = true;
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
