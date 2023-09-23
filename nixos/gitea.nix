{ config, makeEnable, ... }:
makeEnable config "modules.gitea" false {
  age.secrets."gitea-runner-token".file = ./secrets/gitea-runner-token.age;

  services.gitea = {
    enable = true;
    lfs.enable = true;
    settings.server = {
      DOMAIN = "1896Folsom.duckdns.org";
      SSH_PORT = 1123;
    };
  };

  services.gitea-actions-runner.instances-nix-runner = {
    enable = true;
    url = config.services.gitea.settings.server.ROOT_URL;
    tokenFile = config.age.secrets.gitea-runner-token.path;
    labels = [ "nixos:host" ];
  };
}
