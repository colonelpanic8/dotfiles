{ config, makeEnable, ... }:
makeEnable config "modules.gitea" false {

  services.gitea = {
    enable = true;
    lfs.enable = true;
    settings.server = {
      DOMAIN = "dev.railbird.ai";
      SSH_PORT = 1123;
      HTTP_PORT = 3001;
      ROOT_URL = "https://dev.railbird.ai";
    };
    settings.actions = {
      ENABLED = true;
    };
  };

  services.nginx = {
    enable = true;
    virtualHosts = {
      "dev.railbird.ai" = {
        serverName = "dev.railbird.ai";
        enableACME = true;
        forceSSL = true;
        listen = [{
          addr = "0.0.0.0";
          ssl = true;
        }];
        locations."/" = {
          proxyPass = "http://localhost:3001";
        };
      };
    };
  };
}
