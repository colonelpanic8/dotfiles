{ forEachUser, ... }:
let biskcomp-nginx-hostnames = "192.168.1.44 railbird.ai 1896Folsom.duckdns.org biskcomp.local 0.0.0.0 67.162.131.71";
in
{
  imports = [
    ../configuration.nix
    ../raspberry-pi.nix
  ];

  modules.raspberry-pi.enable = true;

  modules.base.enable = true;
  modules.desktop.enable = true;
  modules.xmonad.enable = true;
  modules.extra.enable = false;
  modules.code.enable = true;
  modules.games.enable = false;
  modules.syncthing.enable = true;
  modules.fonts.enable = true;
  modules.nixified-ai.enable = false;
  modules.cache-server = {
    enable = true;
    host-string = biskcomp-nginx-hostnames;
    port = 80;
    path = "/nix-cache";
  };

  services.vaultwarden = {
    enable = true;
    config = {
      ROCKET_ADDRESS = "::1";
      ROCKET_PORT = 8222;
    };
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedTlsSettings = true;
    virtualHosts = {
      "192.168.1.44 railbird.ai 1896Folsom.duckdns.org 0.0.0.0 67.162.131.71" = {
        root = ../railbird.ai;
        locations."/" = {
          index = "index.html";
        };
      };
      # Server block for Vaultwarden on a different port
      "_:8222" = {
        listen = [ { addr = "::"; port = 8222; } ];  # Listen on IPv6 and port 8222
        forceSSL = false;  # Assuming you're not using HTTPS for this one
        locations."/" = {
          proxyPass = "http://::1:8222";
          proxySetHeaders = {
            Host = "$host";
            X-Real-IP = "$remote_addr";
            X-Forwarded-For = "$proxy_add_x_forwarded_for";
            X-Forwarded-Proto = "$scheme";
          };
          proxyRedirect = "off";
        };
      };
    };
  };

  services.plex = {
    enable = true;
  };

  services.gitea = {
    enable = true;
    lfs.enable = true;
    settings.server = {
      DOMAIN = "1896Folsom.duckdns.org";
      SSH_PORT = 1123;
    };
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
