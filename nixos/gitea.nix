{ config, makeEnable, ... }:
makeEnable config "modules.gitea" false {

  services.gitea = {
    enable = true;
    lfs.enable = true;
    settings.server = {
      DOMAIN = "1896Folsom.duckdns.org";
      SSH_PORT = 1123;
      HTTP_PORT = 3001;
      ROOT_URL = "https://1896Folsom.duckdns.org:3000";
    };
    settings.actions = {
      ENABLED = true;
    };
  };

  services.nginx = {
    enable = true;
    virtualHosts = {
      "gitea" = {
        serverName = "1896Folsom.duckdns.org";
        # enableACME = true;
        # forceSSL = true;
        listen = [{
          addr = "0.0.0.0";
          port = 3000;
          ssl = true;
        }];
        locations."/" = {
          proxyPass = "http://localhost:3001";
        };
      };
    };
  };
}
