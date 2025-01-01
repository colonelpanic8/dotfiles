{ config, makeEnable, ... }:
makeEnable config "myModules.gitea" false {

  services.gitea = {
    enable = true;
    lfs.enable = true;
    dump = {
      enable = true;
      interval = "23:05";
    };
    disableRegistration = true;
    settings.service = {
      DISABLE_REGISTRATION = true;
    };
    settings.server = {
      DOMAIN = "dev.railbird.ai";
      SSH_PORT = 1123;
      HTTP_PORT = 3001;
      ROOT_URL = "https://dev.railbird.ai";
      DISABLE_REGISTRATION = true;
    };
    settings.actions = {
      ENABLED = true;
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
