{ config, makeEnable, ... }:
makeEnable config "modules.gitea" false {

  services.gitea = {
    enable = true;
    lfs.enable = true;
    settings.server = {
      DOMAIN = "1896Folsom.duckdns.org";
      SSH_PORT = 1123;
      HTTP_PORT = 3000;
    };
    settings.actions = {
      ENABLED = true;
    };
  };

  services.nginx = {
    enable = true;
    virtualHosts = {
      "1896Folsom.duckdns.org:3001" = {
        enableACME = true;
        listen = [{
          addr = "0.0.0.0";
          port = 3001;
        }];
        # enableACME = true;
        # forceSSL = true;
        locations."/" = {
          proxyPass = "http://localhost:3000";
        };
      };
    };
  };
}
