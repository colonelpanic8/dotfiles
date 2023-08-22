{ config, makeEnable, ... }:
makeEnable config "modules.cache-server" false {
  age.secrets."cache-priv-key.pem".file = ./secrets/cache-priv-key.pem.age;

  services.nix-serve = {
    enable = true;
    secretKeyFile = config.age.secrets."cache-priv-key.pem".path;
    port = 5050;
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    virtualHosts = {
      "0.0.0.0" = {
        locations."/".proxyPass = "http://${config.services.nix-serve.bindAddress}:${toString config.services.nix-serve.port}";
      };
    };
  };
}
