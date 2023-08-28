{ config, makeEnable, lib, ... }:
with lib;
let cfg = config.modules.cache-server;
in
{
  options = {
    modules.cache-server = {
      enable = mkEnableOption "nix cache server";
      port = mkOption {
        type = types.int;
        default = 8080;
      };
      host-string = mkOption {
        type = types.string;
        default = "0.0.0.0";
      };
      path = mkOption {
        type = types.string;
        default = "/";
      };
    };
  };

  config = mkIf cfg.enable {
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
        "${cfg.host-string}" = {
          locations."${cfg.path}".proxyPass = "http://${config.services.nix-serve.bindAddress}:${toString config.services.nix-serve.port}";
          listen = [ { addr = "0.0.0.0"; port = cfg.port; } ];
        };
      };
    };
  };
}

