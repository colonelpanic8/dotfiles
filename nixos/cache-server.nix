{ config, lib, ... }:
with lib;
let cfg = config.modules.cache-server;
in
{
  options = {
    modules.cache-server = {
      enable = mkEnableOption "nix cache server";
      port = mkOption {
        type = types.int;
        default = 5050;
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
      port = cfg.port;
    };
  };
}

