{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.myModules.cache-server;
in {
  options = {
    myModules.cache-server = {
      enable = mkEnableOption "nix cache server";
      port = mkOption {
        type = types.int;
        default = 5050;
      };
      host-string = mkOption {
        type = types.str;
        default = "0.0.0.0";
      };
      path = mkOption {
        type = types.str;
        default = "/";
      };
    };
  };

  config = mkIf cfg.enable {
    age.secrets."cache-priv-key.pem".file = ./secrets/cache-priv-key.pem.age;

    # harmonia rather than nix-serve: nix-serve's prefork workers were each
    # held by one slow client's parallel downloads, stalling every host.
    services.harmonia.cache = {
      enable = true;
      signKeyPaths = [config.age.secrets."cache-priv-key.pem".path];
      settings = {
        bind = "${cfg.host-string}:${toString cfg.port}";
        priority = 30;
      };
    };
  };
}
