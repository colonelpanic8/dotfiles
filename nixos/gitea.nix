{ config, makeEnable, ... }:
makeEnable config "modules.gitea" false {

  services.gitea = {
    enable = true;
    lfs.enable = true;
    dump = {
      enable = true;
      interval = "23:05";
    };
    settings.server = {
      DOMAIN = "dev.railbird.ai";
      SSH_PORT = 1123;
      HTTP_PORT = 3001;
      ROOT_URL = "https://dev.railbird.ai";
    };
    settings.actions = {
      ENABLED = true;
      DISABLE_REGISTRATION = true;
    };
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedTlsSettings = true;
    virtualHosts = {
      "dev.railbird.ai" = {
        serverName = "dev.railbird.ai";
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://localhost:3001";
        };
      };
    };
  };
}
