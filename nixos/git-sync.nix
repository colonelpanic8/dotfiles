{ pkgs, lib, ... }:
let
  gitSyncTrayEnv = {
    Service.Environment = [ "GIT_SYNC_TRAY=1" ];
  };
in {
  home-manager.users.imalison = ({ config, ... }: {
    services.git-sync = {
      enable = true;
      package = pkgs.git-sync-rs;
      repositories = {
        config = {
          path = config.home.homeDirectory + "/config";
          uri = "git@github.com:IvanMalison/config.git";
        };
        org = {
          path = config.home.homeDirectory + "/org";
          uri = "git@github.com:IvanMalison/org.git";
          interval = 30;
        };
        password-store = {
          path = config.home.homeDirectory + "/.password-store";
          uri = "git@github.com:IvanMalison/.password-store.git";
        };
        katnivan = {
          path = config.home.homeDirectory + "/katnivan";
          uri = "ssh://gitea@dev.railbird.ai:1123/colonelpanic/katnivan.git";
          interval = 30;
        };
      };
    };

    systemd.user.services = lib.mapAttrs'
      (name: _: lib.nameValuePair "git-sync-${name}" gitSyncTrayEnv)
      config.services.git-sync.repositories;
  });

  home-manager.users.kat = ({ config, ... }: {
    services.git-sync = {
      enable = true;
      repositories = {
        obsidian = {
          path = config.home.homeDirectory + "/obsidian";
          uri = "git@github.com:katandtonic/obsidian.git";
        };
        org = {
          path = config.home.homeDirectory + "/org";
          uri = "ssh://gitea@1896Folsom.duckdns.org:1123/kkathuang/org.git";
          interval = 180;
        };
        katnivan = {
          path = config.home.homeDirectory + "/katnivan";
          uri = "ssh://gitea@1896Folsom.duckdns.org:1123/colonelpanic/katnivan.git";
          interval = 30;
        };
      };
    };
  });
}
